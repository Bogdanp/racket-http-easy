#lang racket/base

(require net/http-client
         net/uri-codec
         net/url
         racket/contract
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
 session-shutdown!
 session-request)

(define method/c
  (or/c 'delete 'head 'get 'options 'patch 'post 'put symbol?))

(struct session (pool)
  #:transparent)

(define/contract (make-session url-or-string [conf (make-pool-config)])
  (->* ((or/c string? url?)) (pool-config?) session?)
  (define u (if (url? url-or-string) url-or-string (string->url url-or-string)))
  (define connector (make-url-connector u))
  (define pool (make-pool conf connector))
  (define s (session pool))
  (begin0 s
    (will-register executor s session-shutdown!)
    (log-http-easy-debug "session ready")))

(define/contract (session-shutdown! s)
  (-> session? void?)
  (pool-shutdown! (session-pool s))
  (log-http-easy-debug "session shut down"))

;; TODO: Send timeouts.
(define/contract (session-request s path
                                  #:method [method 'get]
                                  #:headers [headers (hasheq)]
                                  #:params [params null]
                                  #:timeouts [timeouts (make-timeout-config)]
                                  #:max-attempts [max-attempts 3])
  (->* (session? non-empty-string?)
       (#:method method/c
        #:headers (hash/c symbol? (or/c bytes? string?))
        #:params (listof (cons/c symbol? string?))
        #:timeouts timeout-config?
        #:max-attempts exact-positive-integer?)
       response?)
  (unless (string-prefix? path "/")
    (raise-argument-error 'path "an absolute path" path))

  (define pool (session-pool s))
  (define path-with-query
    (cond
      [(null? params) path]
      [else (string-append path "?" (alist->form-urlencoded params))]))

  (let loop ([attempts 1])
    (define conn (pool-lease (session-pool s) timeouts))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-http-easy-warning "connection failed: ~a" (exn-message e))
                       (http-conn-close! conn)
                       (pool-release pool conn)
                       (cond
                         [(< attempts max-attempts)
                          (log-http-easy-debug "retrying~n  attempts: ~a/~a" attempts max-attempts)
                          (loop (add1 attempts))]

                         [else
                          (raise e)]))])
      (define-values (resp-status resp-headers resp-out)
        (http-conn-sendrecv!
         conn path-with-query
         #:method (method->bytes method)
         #:headers (headers->list headers)))
      (log-http-easy-debug "response: ~.s" resp-status)
      (define resp
        (make-response resp-status
                       resp-headers
                       resp-out
                       (lambda (_)
                         (pool-release (session-pool s) conn))))
      (begin0 resp
        (will-register executor resp response-close!)))))


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
