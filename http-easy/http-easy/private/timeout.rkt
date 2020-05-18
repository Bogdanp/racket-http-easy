#lang racket/base

(require racket/contract)

(provide
 timeout/c
 make-timeout-config
 timeout-config?
 timeout-config-lease
 timeout-config-connect
 timeout-config-send)

(define timeout/c
  (or/c false/c (and/c real? positive?)))

(struct timeout-config (lease connect send)
  #:transparent)

(define/contract (make-timeout-config #:lease [lease 5]
                                      #:connect [connect 5]
                                      #:send [send 30])
  (->* ()
       (#:lease timeout/c
        #:connect timeout/c
        #:send timeout/c)
       timeout-config?)
  (timeout-config lease connect send))
