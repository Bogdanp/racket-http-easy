#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         net/url
         racket/contract
         "http-easy/private/common.rkt"
         "http-easy/private/error.rkt"
         "http-easy/private/pool.rkt"
         "http-easy/private/response.rkt"
         "http-easy/private/session.rkt"
         "http-easy/private/timeout.rkt")

(provide
 exn:fail:http-easy?
 exn:fail:http-easy:timeout?
 exn:fail:http-easy:timeout-kind

 limit/c
 make-pool-config
 pool-config?

 timeout/c
 make-timeout-config
 timeout-config?

 (all-from-out "http-easy/private/response.rkt")
 (all-from-out "http-easy/private/session.rkt")

 current-session)

(define/contract current-session
  (parameter/c session?)
  (make-parameter (make-session)))

(define (make-requester method)
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (when (memq '#:method kws)
       (raise-user-error "#:method keyword argument not allowed"))
     (keyword-apply session-request kws kw-args (current-session) args #:method method))))

(define-syntax (define-requesters stx)
  (syntax-parse stx
    [(_ method:id ...+)
     #'(begin
         (define method (make-requester 'method)) ...
         (provide method ...))]))

(define-requesters delete head get options patch post put)
