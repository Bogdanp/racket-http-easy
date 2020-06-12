#lang racket/base

(require racket/contract)

(provide
 timeout/c
 make-timeout-config
 timeout-config?
 timeout-config-lease
 timeout-config-connect
 timeout-config-request)

(define timeout/c
  (or/c false/c (and/c real? positive?)))

(struct timeout-config (lease connect request)
  #:transparent)

(define/contract (make-timeout-config #:lease [lease 5]
                                      #:connect [connect 5]
                                      #:request [request 30])
  (->* ()
       (#:lease timeout/c
        #:connect timeout/c
        #:request timeout/c)
       timeout-config?)
  (timeout-config lease connect request))
