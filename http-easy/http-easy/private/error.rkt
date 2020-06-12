#lang racket/base

(require racket/contract)

(provide
 make-timeout-error
 exn:fail:http-easy?
 exn:fail:http-easy:timeout?
 exn:fail:http-easy:timeout-kind)

(struct exn:fail:http-easy exn:fail ())
(struct exn:fail:http-easy:timeout exn:fail:http-easy (kind))

(define/contract (make-timeout-error kind)
  (-> (or/c 'lease 'connect 'request) exn:fail:http-easy:timeout?)
  (exn:fail:http-easy:timeout "timed out" (current-continuation-marks) kind))
