#lang racket/base

(require json
         racket/contract
         racket/match
         racket/port
         "common.rkt")

(provide
 status-code/c
 make-response
 response?
 response-status-line
 response-http-version
 response-status-code
 response-status-message
 response-headers
 response-headers-ref
 response-headers-ref/all
 response-output
 response-body
 response-json
 response-close!)

(struct response
  (sema
   status-line
   http-version
   status-code
   status-message
   headers
   output
   [body-data #:mutable]
   closer
   [closed? #:mutable]))

(define response-closer/c
  (-> response? void?))

(define status-code/c
  (integer-in 100 999))

(define/contract (make-response status headers out closer)
  (-> bytes? (listof bytes?) input-port? response-closer/c response?)
  (match status
    [(regexp #rx"^HTTP/(...) ([1-9][0-9][0-9]) (.*)$"
             (list status-line
                   http-version
                   (app bytes->number status-code)
                   status-message))
     (response (make-semaphore 1)
               status-line
               http-version
               status-code
               status-message
               headers
               out
               #f
               closer
               #f)]

    [_
     (raise-argument-error 'status "a valid status line" status)]))

(define-syntax-rule (define-headers-ref id for-form)
  (define/contract (id r h)
    (-> response? symbol? any/c)
    (define h:bs (symbol->bytes h))
    (define re (byte-regexp (bytes-append #"^(?i:" h:bs #"): ")))
    (for-form ([header (in-list (response-headers r))]
               #:when (regexp-match re header))
      (subbytes header (+ 2 (bytes-length h:bs))))))

(define-headers-ref response-headers-ref for/first)
(define-headers-ref response-headers-ref/all for/list)

(define/contract (response-body r)
  (-> response? bytes?)
  (call-with-semaphore (response-sema r)
    (lambda ()
      (cond
        [(response-body-data r) => values]
        [else
         (define body (port->bytes (response-output r)))
         (begin0 body
           (set-response-body-data! r body))]))))

(define/contract (response-json r)
  (-> response? jsexpr?)
  (bytes->jsexpr (response-body r)))

(define/contract (response-close! r)
  (-> response? void?)
  (call-with-semaphore (response-sema r)
    (lambda ()
      (unless (response-closed? r)
        (copy-port (response-output r) (open-output-nowhere))
        ((response-closer r) r)
        (set-response-closed?! r #t)))))
