#lang racket/base

(require racket/contract/base)

(provide
 timeout/c
 timeout-config?
 timeout-config-lease
 timeout-config-connect
 timeout-config-request
 (contract-out
  [make-timeout-config (->* ()
                            (#:lease timeout/c
                             #:connect timeout/c
                             #:request timeout/c)
                            timeout-config?)]
  [make-request-timeout-evt (-> timeout-config? evt?)]))

(define timeout/c
  (or/c #f (and/c real? positive?)))

(struct timeout-config (lease connect request)
  #:transparent)

(define (make-timeout-config #:lease [lease 5]
                             #:connect [connect 5]
                             #:request [request 30])
  (timeout-config lease connect request))

(define (make-request-timeout-evt t)
  (alarm-evt
   (+ (current-inexact-monotonic-milliseconds)
      (* (timeout-config-request t) 1000))
   #t))
