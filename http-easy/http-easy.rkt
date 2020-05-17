#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         net/url
         racket/contract
         "http-easy/private/common.rkt"
         "http-easy/private/error.rkt"
         "http-easy/private/response.rkt"
         "http-easy/private/session.rkt"
         "http-easy/private/timeout.rkt")

(provide
 exn:fail:http-easy?
 exn:fail:http-easy:timeout?
 exn:fail:http-easy:timeout-kind

 make-timeout-config

 (all-from-out "http-easy/private/error.rkt")
 (all-from-out "http-easy/private/response.rkt")
 (all-from-out "http-easy/private/session.rkt"))

(define/contract ((make-requester method) url-or-string
                                          #:headers [headers (hasheq)]
                                          #:params [params null]
                                          #:max-attempts [max-attempts 3])
  (-> symbol? (->* ((or/c string? url?))
                   (#:headers (hash/c symbol? (or/c bytes? string?))
                    #:params (listof (cons/c symbol? string?))
                    #:max-attempts exact-positive-integer?)
                   response?))
  (define u (if (url? url-or-string) url-or-string (string->url url-or-string)))
  (define p (url-path-string u))
  (define s (make-session u))
  (session-request s p
                   #:method method
                   #:headers headers
                   #:params (append (url-query u) params)
                   #:max-attempts max-attempts))

(define-syntax (define-requesters stx)
  (syntax-parse stx
    [(_ method:id ...+)
     #'(begin
         (define method (make-requester 'method)) ...
         (provide method ...))]))

(define-requesters delete head get options patch post put)
