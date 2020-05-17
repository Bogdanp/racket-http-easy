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

 (all-from-out "http-easy/private/response.rkt")
 (all-from-out "http-easy/private/session.rkt"))

(define (make-requester method)
  (make-keyword-procedure
   (lambda (kws kw-args url-or-string . args)
     (define u (if (url? url-or-string) url-or-string (string->url url-or-string)))
     (define p (url-path-string u))
     (define s (make-session u))
     (keyword-apply session-request kws kw-args s p args #:method method))))

(define-syntax (define-requesters stx)
  (syntax-parse stx
    [(_ method:id ...+)
     #'(begin
         (define method (make-requester 'method)) ...
         (provide method ...))]))

(define-requesters delete head get options patch post put)
