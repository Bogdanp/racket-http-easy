#lang racket/base

(require json
         net/http-client
         net/uri-codec
         net/url
         openssl
         racket/contract
         racket/format
         racket/match
         "common.rkt"
         "contract.rkt"
         "logger.rkt"
         "pool.rkt"
         "response.rkt"
         "timeout.rkt")

(provide
 make-session
 session?
 session-close!
 session-request)

(define default-pool-config (make-pool-config))

(struct session (sema conf pools ssl-ctx [closed? #:mutable])
  #:transparent)

(define/contract (make-session #:pool-config [conf default-pool-config]
                               #:ssl-context [ssl-ctx (ssl-secure-client-context)])
  (->* ()
       (#:pool-config pool-config?
        #:ssl-context ssl-client-context?)
       session?)
  (define s (session (make-semaphore 1) conf (make-hash) ssl-ctx #f))
  (begin0 s
    (will-register executor s session-close!)
    (log-http-easy-debug "session opened")))

(define/contract (session-close! s)
  (-> session? void?)
  (call-with-semaphore (session-sema s)
    (lambda ()
      (unless (session-closed? s)
        (for ([p (in-hash-values (session-pools s))])
          (pool-close! p))
        (set-session-closed?! s #t)
        (log-http-easy-debug "session closed")))))

(define (session-lease s url timeouts)
  (define k (pool-key url))
  (define ps (session-pools s))
  (define p
    (call-with-semaphore (session-sema s)
      (lambda ()
        (hash-ref! ps k (lambda ()
                          (define ssl-ctx (session-ssl-ctx s))
                          (define connector (make-url-connector url ssl-ctx))
                          (make-pool (session-conf s) connector))))))

  (pool-lease p timeouts))

(define (session-release s url c)
  (define k (pool-key url))
  (define ps (session-pools s))
  (define p
    (call-with-semaphore (session-sema s)
      (lambda ()
        (hash-ref ps k #f))))

  (when p
    (pool-release p c)))

(define (pool-key u)
  (~a (or (url-scheme u)
          (case (url-port u)
            [(443) "https"]
            [else "http"]))
      "://"
      (url-host u)
      ":"
      (or (url-port u)
          (case (url-scheme u)
            [("https") 443]
            [else 80]))))

(define default-timeout-config (make-timeout-config))

(define unsupplied
  (let ()
    (struct unsupplied ())
    (values (unsupplied))))

(define (supplied? v)
  (not (eq? v unsupplied)))

;; TODO: Write timeouts.
;; TODO: Read timeouts.
(define/contract (session-request s
                                  urlish
                                  #:drain? [drain? #t]
                                  #:close? [close? #f]
                                  #:method [method 'get]
                                  #:headers [headers (hasheq)]
                                  #:params [params null]
                                  #:auth [auth #f]
                                  #:data [data #f]
                                  #:form [form unsupplied]
                                  #:json [json unsupplied]
                                  #:timeouts [timeouts default-timeout-config]
                                  #:max-attempts [max-attempts 3]
                                  #:max-redirects [max-redirects 16])
  (->* (session? (or/c bytes? string? url?))
       (#:drain? boolean?
        #:close? boolean?
        #:method method/c
        #:headers headers/c
        #:params query-params/c
        #:auth (or/c false/c auth-procedure/c)
        #:data (or/c false/c bytes? string? input-port?)
        #:form query-params/c
        #:json jsexpr?
        #:timeouts timeout-config?
        #:max-attempts exact-positive-integer?
        #:max-redirects exact-nonnegative-integer?)
       response?)

  (define (request u
                   #:attempts [attempts 1]
                   #:method [method method]
                   #:headers [headers headers]
                   #:params [params params]
                   #:auth [auth auth]
                   #:data [data data]
                   #:form [form form]
                   #:json [json json]
                   #:history [history null]
                   #:redirects [redirects-remaining max-redirects])
    (define-values (headers* params*)
      (if auth
          (auth u headers params)
          (values headers params)))
    (define path&query (make-path&query u params*))
    (define c (session-lease s u timeouts))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-http-easy-warning "connection failed: ~a" (exn-message e))
                       (session-release s u c)
                       (cond
                         [(< attempts max-attempts)
                          (log-http-easy-debug "retrying~n  attempts: ~a/~a" attempts max-attempts)
                          (request u path&query
                                   #:attempts (add1 attempts)
                                   #:history history)]

                         [else
                          (raise e)]))])
      (define-values (resp-status resp-headers resp-out)
        (http-conn-sendrecv!
         c path&query
         #:close? close?
         #:method (method->bytes method)
         #:headers (headers->list
                    (cond
                      [(supplied? json) (hash-set headers* 'content-type #"application/json; charset=utf-8")]
                      [(supplied? form) (hash-set headers* 'content-type #"application/x-www-form-urlencoded; charset=utf-8")]
                      [else headers*]))
         #:data (cond
                  [(supplied? json) (jsexpr->bytes json)]
                  [(supplied? form) (alist->form-urlencoded form)]
                  [(input-port? data) (port->data-procedure data)]
                  [else data])))
      (log-http-easy-debug "response: ~.s" resp-status)
      (define resp
        (make-response resp-status
                       resp-headers
                       resp-out
                       history
                       (lambda (_)
                         (session-release s u c))))

      (cond
        [(and (positive? redirects-remaining) (redirect? resp))
         (define location:bs (response-headers-ref resp 'location))
         (define u* (ensure-absolute-url (->url location:bs) u))
         (log-http-easy-debug "following ~s redirect to ~.s" (response-status-code resp) location:bs)
         (response-drain! resp)
         (response-close! resp)
         (request u*
                  #:method (case (response-status-code resp)
                             [(301 302) 'get]
                             [(303)     'get]
                             [(307)     method])
                  #:headers (hash-remove headers 'authorization)
                  #:auth (if (same-origin? u* u) auth #f)
                  #:history (cons resp history)
                  #:redirects (sub1 redirects-remaining))]

        [(or close? drain?)
         (begin0 resp
           (response-drain! resp)
           (response-close! resp))]

        [else
         (begin0 resp
           (will-register executor resp response-close!))])))

  (request (->url urlish)))

(define (->url urlish)
  (cond
    [(url? urlish) urlish]
    [(bytes? urlish) (string->url (bytes->string/utf-8 urlish))]
    [else (string->url urlish)]))

(define (make-path&query u params)
  (define path (url-path-string u))
  (define all-params (append (url-query u) params))
  (cond
    [(null? all-params) path]
    [else (string-append path "?" (alist->form-urlencoded all-params))]))

(define (ensure-absolute-url u* u)
  (cond
    [(and (url-scheme u*)
          (url-host u*)
          (url-port u*))
     u*]

    [else
     (struct-copy url u*
                  [scheme (url-scheme u)]
                  [host (url-host u)]
                  [port (url-port u)])]))

(define (same-origin? a b)
  (and (equal? (url-scheme a) (url-scheme b))
       (equal? (url-host a) (url-host b))
       (equal? (url-port a) (url-port b))))

(define (redirect? resp)
  (case (response-status-code resp)
    [(301 302 303 307) (response-headers-ref resp 'location)]
    [else #f]))


;; GC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define executor
  (make-will-executor))

(void
 (thread
  (lambda ()
    (let loop ()
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (log-http-easy-warning "will execution failed: ~a" (exn-message e))
                         (loop))])
        (will-execute executor)
        (loop))))))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((make-url-connector u ssl-ctx) conn)
  (match-define (struct* url ([scheme s] [host h] [port p])) u)
  (begin0 conn
    (if (http-conn-live? conn)
        (http-conn-enliven! conn)
        (http-conn-open! conn h
                         #:port (or p (if (equal? s "https") 443 80))
                         #:ssl? (and (equal? s "https") ssl-ctx)
                         #:auto-reconnect? #t))))

(define (headers->list headers)
  (for/list ([(name value) (in-hash headers)])
    (bytes-append (symbol->bytes name) #": " (cond
                                               [(bytes? value) value]
                                               [else (string->bytes/utf-8 value)]))))

(define ((port->data-procedure inp) write-chunk)
  (define buf (make-bytes (* 16 1024)))
  (let loop ()
    (define n-read (read-bytes-avail! buf inp))
    (unless (eof-object? n-read)
      (write-chunk (subbytes buf 0 n-read))
      (loop))))
