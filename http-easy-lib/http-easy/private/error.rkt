#lang racket/base

(require racket/contract/base)

(provide
 exn:fail:http-easy?
 exn:fail:http-easy:timeout?
 exn:fail:http-easy:timeout-kind

 (contract-out
  [oops
   (-> symbol? string? any/c ... void?)]
  [make-error
   (-> string? any/c ... exn:fail:http-easy?)]
  [make-timeout-error
   (-> (or/c 'lease 'connect 'request) exn:fail:http-easy:timeout?)]))

(struct exn:fail:http-easy exn:fail ())
(struct exn:fail:http-easy:timeout exn:fail:http-easy (kind))

(define (oops who msg . args)
  (raise (apply make-error (format "~a: ~a" who msg) args)))

(define (make-error msg . args)
  (exn:fail:http-easy
   (apply format msg args)
   (current-continuation-marks)))

(define (make-timeout-error kind)
  (exn:fail:http-easy:timeout
   (format "~a timed out" kind)
   (current-continuation-marks)
   kind))
