#lang racket/base

(require net/url
         racket/contract/base
         "contract.rkt"
         "response.rkt")

(provide
 middleware/c
 middleware-continuation/c
 (contract-out
  [compose-middleware
   (case->
    (-> middleware/c middleware/c)
    (-> middleware/c middleware/c middleware/c)
    (-> middleware/c middleware/c #:rest middleware/c middleware/c))]))

(define middleware-continuation/c
  (-> url?
      #:method method/c
      #:headers headers/c
      #:params query-params/c
      #:auth (or/c #f auth-procedure/c)
      #:data (or/c #f bytes? string? input-port? payload-procedure/c)
      #:history (listof response?)
      #:attempts exact-nonnegative-integer?
      #:redirects exact-nonnegative-integer?
      response?))

(define middleware/c
  (-> url?
      middleware-continuation/c
      #:method method/c
      #:headers headers/c
      #:params query-params/c
      #:auth (or/c #f auth-procedure/c)
      #:data (or/c #f bytes? string? input-port? payload-procedure/c)
      #:history (listof response?)
      #:attempts exact-nonnegative-integer?
      #:redirects exact-nonnegative-integer?
      response?))

(define compose-middleware
  (case-lambda
    [(a) a]
    [(a b)
     (make-keyword-procedure
      (lambda (kws kw-args u go . args)
        (keyword-apply
         a kws kw-args u
         (make-keyword-procedure
          (lambda (kws kw-args u . args) ;; noqa
            (keyword-apply b kws kw-args u go args)))
         args)))]
    [(a b . args)
     (apply compose-middleware (compose-middleware a b) args)]))
