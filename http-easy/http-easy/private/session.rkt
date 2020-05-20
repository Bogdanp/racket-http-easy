#lang racket/base

(require net/http-client
         net/uri-codec
         net/url
         racket/contract
         racket/format
         racket/match
         racket/string
         "common.rkt"
         "logger.rkt"
         "pool.rkt"
         "response.rkt"
         "timeout.rkt")

(provide
 method/c
 make-session
 session?
 session-close!
 session-request)

(define method/c
  (or/c 'delete 'head 'get 'options 'patch 'post 'put symbol?))

(struct session (sema conf pools [closed? #:mutable])
  #:transparent)

(define/contract (make-session [conf (make-pool-config)])
  (->* () (pool-config?) session?)
  (define s (session (make-semaphore 1) conf (make-hash) #f))
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
                          (define connector (make-url-connector url))
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
          (if (equal? (url-port u) 443)
              "https"
              "http"))
      "://"
      (url-host u)
      ":"
      (or (url-port u)
          (if (equal? (url-scheme u) "https")
              443
              80))))

;; TODO: Write timeouts.
;; TODO: Read timeouts.
(define/contract (session-request s
                                  string-or-url
                                  #:drain? [drain? #t]
                                  #:close? [close? #f]
                                  #:method [method 'get]
                                  #:headers [headers (hasheq)]
                                  #:params [params null]
                                  #:timeouts [timeouts (make-timeout-config)]
                                  #:max-attempts [max-attempts 3])
  (->* (session? (or/c string? url?))
       (#:drain? boolean?
        #:close? boolean?
        #:method method/c
        #:headers (hash/c symbol? (or/c bytes? string?))
        #:params (listof (cons/c symbol? string?))
        #:timeouts timeout-config?
        #:max-attempts exact-positive-integer?)
       response?)

  (define u (if (url? string-or-url) string-or-url (string->url string-or-url)))
  (define path (url-path-string u))
  (define path-with-query
    (if (null? params)
        path
        (string-append path "?" (alist->form-urlencoded params))))

  (let loop ([attempts 1])
    (define c (session-lease s u timeouts))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-http-easy-warning "connection failed: ~a" (exn-message e))
                       (session-release s u c)
                       (cond
                         [(< attempts max-attempts)
                          (log-http-easy-debug "retrying~n  attempts: ~a/~a" attempts max-attempts)
                          (loop (add1 attempts))]

                         [else
                          (raise e)]))])
      (define-values (resp-status resp-headers resp-out)
        (http-conn-sendrecv!
         c path-with-query
         #:close? close?
         #:method (method->bytes method)
         #:headers (headers->list headers)))
      (log-http-easy-debug "response: ~.s" resp-status)
      (define resp
        (make-response resp-status
                       resp-headers
                       resp-out
                       (lambda (_)
                         (session-release s u c))))
      (cond
        [drain?
         (begin0 resp
           (response-drain! resp)
           (response-close! resp))]

        [close?
         (begin0 resp
           (response-close! resp))]

        [else
         (begin0 resp
           (will-register executor resp response-close!))]))))


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

(define ((make-url-connector u) conn)
  (match-define (struct* url ([scheme s] [host h] [port p])) u)
  (begin0 conn
    (if (http-conn-live? conn)
        (http-conn-enliven! conn)
        (http-conn-open! conn h
                         #:port (or p (if (equal? s "https") 443 80))
                         #:ssl? (equal? s "https")
                         #:auto-reconnect? #t))))

(define (headers->list headers)
  (for/list ([(name value) (in-hash headers)])
    (bytes-append (symbol->bytes name) #": " (cond
                                               [(bytes? value) value]
                                               [else (string->bytes/utf-8 value)]))))
