#lang racket/base

(require racket/async-channel
         racket/match
         racket/tcp
         web-server/servlet-dispatch
         web-server/web-server)

(provide
 call-with-tcp-server
 call-with-web-server)

(define (call-with-tcp-server handle proc)
  (define listener
    (tcp-listen 0 128 "127.0.0.1"))
  (define-values (_host port _remote-host _remote-port)
    (tcp-addresses listener #t))
  (define stop-ch (make-channel))
  (thread
   (lambda ()
     (let connection-loop ()
       (sync
        (handle-evt
         stop-ch
         (lambda (_)
           (void)))
        (handle-evt
         (tcp-accept-evt listener)
         (lambda (ports)
           (match-define (list in out) ports)
           (let loop ([lines null])
             (define line
               (read-line in 'return-linefeed))
             (cond
               [(or (eof-object? line)
                    (string=? line ""))
                (tcp-abandon-port in)
                (with-handlers ([exn:fail? (lambda (e)
                                             (log-error "tcp-server handler failed: ~a" (exn-message e)))])
                  (handle (reverse lines) out))]
               [else
                (loop (cons line lines))]))
           (flush-output out)
           (close-output-port out)
           (connection-loop)))))))
  (dynamic-wind
    void
    (lambda ()
      (proc port))
    (lambda ()
      (channel-put stop-ch #t)
      (tcp-close listener))))

(define (call-with-web-server start proc #:port [port 9911])
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
      (proc))
    (lambda ()
      (stop))))
