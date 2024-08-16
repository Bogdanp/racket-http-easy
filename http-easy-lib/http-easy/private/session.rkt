#lang racket/base

(require net/cookies/user-agent
         net/http-client
         net/uri-codec
         net/url
         racket/class
         racket/contract/base
         racket/format
         racket/lazy-require
         racket/match
         racket/promise
         "common.rkt"
         "contract.rkt"
         "error.rkt"
         "logger.rkt"
         "payload.rkt"
         "pool.rkt"
         "proxy.rkt"
         "response.rkt"
         "timeout.rkt"
         "url.rkt"
         "user-agent.rkt")

(lazy-require
 [json (jsexpr?)]
 [openssl (ssl-client-context? ssl-secure-client-context)]
 [racket/unix-socket (unix-socket-connect)])

;; session ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [make-session (->* []
                     [#:pool-config pool-config?
                      #:ssl-context (or/c #f ssl-client-context? (promise/c ssl-client-context?))
                      #:cookie-jar (is-a?/c cookie-jar<%>)
                      #:proxies (listof proxy?)]
                     session?)]
  [session? (-> any/c boolean?)]
  [session-close! (-> session? void?)]
  [session-request (->i ([s session?]
                         [urlish urlish/c])
                        (#:close? [close? boolean?]
                         #:stream? [stream? boolean?]
                         #:method [method method/c]
                         #:headers [headers headers/c]
                         #:params [params query-params/c]
                         #:auth [auth (or/c #f auth-procedure/c)]
                         #:data [data (or/c #f bytes? string? input-port? payload-procedure/c)]
                         #:form [form form-data/c]
                         #:json [json jsexpr?]
                         #:timeouts [timeouts timeout-config?]
                         #:max-attempts [max-attempts exact-positive-integer?]
                         #:max-redirects [max-redirects exact-nonnegative-integer?]
                         #:user-agent [user-agent (or/c bytes? string?)])

                        #:pre/name (data form json)
                        "at most one of the #:data, #:form or #:json keyword arguments"
                        (cond
                          [(supplied? data) (and (unsupplied-arg? form) (unsupplied-arg? json))]
                          [(supplied? form) (and (unsupplied-arg? data) (unsupplied-arg? json))]
                          [(supplied? json) (and (unsupplied-arg? data) (unsupplied-arg? form))]
                          [else #t])

                        [res response?])]))

(struct session
  (cust
   sema
   conf
   pools
   ssl-ctx
   cookies
   proxies
   [closed? #:mutable])
  #:transparent)

(define (make-session #:pool-config [conf (make-pool-config)]
                      #:ssl-context [ssl-ctx (delay (ssl-secure-client-context))]
                      #:cookie-jar [cookies #f]
                      #:proxies [proxies null])
  (define s
    (session
     (make-custodian)
     (make-semaphore 1)
     conf
     (make-hash)
     ssl-ctx
     cookies
     proxies
     #f))
  (begin0 s
    (will-register executor s session-close!)
    (log-http-easy-debug "session opened")))

(define (session-close! s)
  (call-with-semaphore (session-sema s)
    (lambda ()
      (unless (session-closed? s)
        (for ([p (in-hash-values (session-pools s))])
          (pool-close! p))
        (set-session-closed?! s #t)
        (custodian-shutdown-all (session-cust s))
        (log-http-easy-debug "session closed")))))

(define (session-lease s url timeouts)
  (parameterize ([current-custodian (session-cust s)])
    (define k (pool-key url))
    (define p
      (call-with-semaphore (session-sema s)
        (lambda ()
          (hash-ref!
           (session-pools s) k
           (lambda ()
             (log-http-easy-debug "creating pool for key ~a" k)
             (define ssl-ctx (session-ssl-ctx s))
             (define proxies (session-proxies s))
             (define connector (make-url-connector url ssl-ctx proxies))
             (make-pool (session-conf s) connector))))))
    (pool-lease p timeouts)))

(define (session-release s url c)
  (define k (pool-key url))
  (define p
    (call-with-semaphore (session-sema s)
      (lambda ()
        (hash-ref (session-pools s) k #f))))

  (when p
    (log-http-easy-debug "releasing connection to pool ~a" k)
    (pool-release p c)))

(define supplied?
  (compose1 not unsupplied-arg?))

(define (session-request sess
                         urlish
                         #:close? [close? #f]
                         #:stream? [stream? #f]
                         #:method [method 'get]
                         #:headers [headers (hasheq)]
                         #:params [params null]
                         #:auth [auth #f]
                         #:data [data #f]
                         #:form [form the-unsupplied-arg]
                         #:json [json the-unsupplied-arg]
                         #:timeouts [timeouts (make-timeout-config)]
                         #:max-attempts [max-attempts 3]
                         #:max-redirects [max-redirects 16]
                         #:user-agent [user-agent (current-user-agent)])
  (define enable-breaks?
    (break-enabled))
  (define the-data
    (cond
      [(supplied? form) (form-payload form)]
      [(supplied? json) (json-payload json)]
      [else data]))

  (define (go u
              #:method [method method] ;; noqa
              #:headers [headers headers] ;; noqa
              #:params [params params] ;; noqa
              #:auth [auth auth] ;; noqa
              #:data [data the-data] ;; noqa
              #:history [history null]
              #:attempts [attempts-remaining max-attempts]
              #:redirects [redirects-remaining max-redirects])
    (let*-values ([(headers) (hash-set headers 'user-agent user-agent)]
                  [(headers) (maybe-add-cookie-header sess u headers)]
                  [(headers params)
                   (if auth
                       (auth u headers params)
                       (values headers params))]
                  [(headers data)
                   (if (procedure? data)
                       (data headers)
                       (values headers data))])
      (parameterize-break #f
        (define conn (session-lease sess u timeouts))
        (define resp
          (with-handlers ([exn:break?
                           (lambda (e)
                             (log-http-easy-warning "received break")
                             (http-conn-close! conn)
                             (session-release sess u conn)
                             (raise e))]
                          [exn:fail?
                           (lambda (e)
                             (log-http-easy-warning "request failed: ~a" (exn-message e))
                             (http-conn-close! conn)
                             (session-release sess u conn)
                             (cond
                               [(exn:fail:http-easy? e)
                                (log-http-easy-warning "error cannot be retried; bubbling up exception")
                                (raise e)]
                               [(positive? attempts-remaining)
                                (log-http-easy-debug "retrying~n  attempts remaining: ~a" (sub1 attempts-remaining))
                                (parameterize-break enable-breaks?
                                  (go u #:attempts (sub1 attempts-remaining) #:history history))]
                               [else
                                (log-http-easy-warning "out of retries; bubbling up exception")
                                (raise e)]))])
            (define resp-ch
              (make-channel))
            (define thd
              (thread
               (lambda ()
                 (with-handlers ([exn:break? void]
                                 [exn:fail? (λ (e) (channel-put resp-ch e))])
                   (define-values (resp-status resp-headers resp-output)
                     (http-conn-sendrecv!
                      conn (url-path&query u params)
                      #:close? close?
                      #:method (method->bytes method)
                      #:headers (headers->list headers)
                      #:data (if (input-port? data)
                                 (port->data-procedure data)
                                 data)))
                   (channel-put
                    resp-ch
                    (make-response
                     resp-status
                     resp-headers
                     resp-output
                     history
                     (lambda (_)
                       (session-release sess u conn))))))))
            (with-handlers ([exn:break?
                             (lambda (e)
                               (break-thread thd)
                               (raise e))])
              (sync/enable-break
               (handle-evt
                resp-ch
                (lambda (r)
                  (when (exn:fail? r)
                    (raise r))
                  (begin0 r
                    (log-http-easy-debug "response: ~.s" (response-status-line r))
                    (maybe-save-cookies! sess u (response-headers r)))))
               (handle-evt
                (make-request-timeout-evt timeouts)
                (lambda (_)
                  (break-thread thd)
                  (log-http-easy-warning "request timed out~n  method: ~s~n  url: ~.s" method urlish)
                  (raise (make-timeout-error 'request))))))))

        (cond
          [(and (positive? redirects-remaining) (redirect? resp))
           (define location (bytes->string/utf-8 (response-headers-ref resp 'location)))
           (define dest-url (ensure-absolute-url u location))
           (log-http-easy-debug "following ~s redirect to ~.s" (response-status-code resp) location)
           (response-drain! resp)
           (response-close! resp)
           (parameterize-break enable-breaks?
             (go dest-url
                 #:method (case (response-status-code resp)
                            [(301 302 303) 'get]
                            [(307)         method])
                 #:headers (hash-remove headers 'authorization)
                 #:auth (if (same-origin? dest-url u) auth #f)
                 #:history (cons resp history)
                 #:redirects (sub1 redirects-remaining)))]

          [(or close? (not stream?))
           (begin0 resp
             (response-drain! resp)
             (response-close! resp))]

          [else
           (begin0 resp
             (will-register executor resp response-close!))]))))

  (go (->url urlish)))

(define (ensure-absolute-url orig location)
  (define location-url
    (string->url location))
  (cond
    [(url-host location-url) location-url]
    [else (combine-url/relative orig location)]))

(define (same-origin? a b)
  (and (equal? (url-scheme a)
               (url-scheme b))
       (equal? (url-host a)
               (url-host b))
       (equal? (url-port a)
               (url-port b))))

(define (redirect? resp)
  (and (memv (response-status-code resp) '(301 302 303 307))
       (response-headers-ref resp 'location)
       #t))


;; GC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define executor
  (make-will-executor))

(void
 (parameterize ([current-namespace (make-empty-namespace)])
   (thread/suspend-to-kill
    (lambda ()
      (let loop ()
        (with-handlers ([exn:fail? (λ (e) (log-http-easy-warning "will execution failed: ~a" (exn-message e)))])
          (will-execute executor))
        (loop))))))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((make-url-connector u ssl-ctx proxies) conn)
  (let ([ssl-ctx (if (promise? ssl-ctx)
                     (force ssl-ctx)
                     ssl-ctx)])
    (begin0 conn
      (cond
        [(http-conn-live? conn)
         (log-http-easy-debug "reusing connection to ~a" (pool-key u))]

        [else
         (log-http-easy-debug "connecting to ~a" (pool-key u))
         (match-define (struct* url ([scheme scheme] [host host] [port port])) u)
         (case scheme
           [("http+unix")
            (define path (form-urlencoded-decode host))
            (define-values (in out)
              (unix-socket-connect path))
            (http-conn-open! conn "" #:ssl? (list #f in out close-output-port))]

           [else
            (or
             (for/first ([p (in-list proxies)] #:when ((proxy-matches? p) u))
               ((proxy-connect! p) conn u ssl-ctx))
             (http-conn-open! conn host
                              #:port (or port (if (equal? scheme "https") 443 80))
                              #:ssl? (and (equal? scheme "https") ssl-ctx)))])]))))

(define (headers->list headers)
  (for/list ([(k v) (in-hash headers)])
    (bytes-append (symbol->bytes k) #": " (->bytes v))))

(define ((port->data-procedure inp) write-chunk)
  (define buf (make-bytes (* 64 1024)))
  (let loop ()
    (define n-read (read-bytes-avail! buf inp))
    (unless (eof-object? n-read)
      (write-chunk (subbytes buf 0 n-read))
      (loop))))

(define (pool-key u)
  (~a (url-scheme* u) "://" (url-host u) ":" (url-port* u)))

(define (->bytes v)
  (cond
    [(bytes? v) v]
    [else (string->bytes/utf-8 v)]))


;; cookies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maybe-add-cookie-header s u headers)
  (cond
    [(session-cookies s)
     => (lambda (cookie-jar)
          (parameterize ([current-cookie-jar cookie-jar])
            (define hdr (cookie-header u))
            (if hdr (hash-set headers 'cookie hdr) headers)))]

    [else headers]))

(define (maybe-save-cookies! s u headers/raw)
  (define cookie-jar (session-cookies s))
  (when cookie-jar
    (parameterize ([current-cookie-jar cookie-jar])
      (extract-and-save-cookies! headers/raw u))))
