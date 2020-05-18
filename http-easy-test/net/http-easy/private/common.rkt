#lang racket/base

(require racket/async-channel
         web-server/servlet-dispatch
         web-server/web-server)

(provide
 call-with-web-server)

(define (call-with-web-server start f #:port [port 9911])
  (define ch (make-async-channel))
  (define stop
    (serve
     #:port port
     #:dispatch (dispatch/servlet start)
     #:confirmation-channel ch))

  (define maybe-e (sync ch))
  (when (exn:fail? maybe-e)
    (raise maybe-e))

  (dynamic-wind
    void
    (lambda ()
      (f))
    (lambda ()
      (stop))))
