#lang racket/base

(require racket/contract/base)

(provide
 exn:fail:http-easy?
 exn:fail:http-easy:timeout?
 exn:fail:http-easy:timeout-kind

 (contract-out
  [make-timeout-error (-> (or/c 'lease 'connect 'request) exn:fail:http-easy:timeout?)]))

(struct exn:fail:http-easy exn:fail ())
(struct exn:fail:http-easy:timeout exn:fail:http-easy (kind))

(define (make-timeout-error kind)
  (exn:fail:http-easy:timeout "timed out" (current-continuation-marks) kind))
