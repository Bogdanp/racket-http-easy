#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre
                     "common.rkt")
         racket/contract/base
         racket/lazy-require
         racket/match
         racket/port
         racket/promise
         "common.rkt"
         "error.rkt"
         "logger.rkt"
         "port.rkt")

(lazy-require
 [json (bytes->jsexpr
        jsexpr?
        read-json)]
 [xml (document?
       document-element
       read-xml/document
       xexpr?
       xml->xexpr)])

(provide
 status-code/c
 response?
 response-status-line
 response-http-version
 response-status-code
 response-status-message
 response-headers
 response-output
 response-history
 (contract-out
  [make-response (-> bytes? (listof bytes?) input-port? (listof response?) response-closer/c response-destroyer/c response?)]
  [response-body (-> response? bytes?)]
  [response-json (-> response? (or/c eof-object? jsexpr?))]
  [response-xexpr (-> response? xexpr?)]
  [response-xml (-> response? document?)]
  [read-response (-> response? any/c)]
  [read-response-json (-> response? (or/c eof-object? jsexpr?))]
  [read-response-xexpr (-> response? xexpr?)]
  [read-response-xml (-> response? document?)]
  [response-drain! (->* [response?] [(or/c #f (and/c real? (not/c negative?)))] void?)]
  [response-close! (-> response? void?)]))

(struct response
  (status-line
   http-version
   status-code
   status-message
   headers
   output
   [data #:mutable]
   history
   closer
   [closed? #:mutable]
   destroyer))

(define response-closer/c
  (-> response? void?))

(define response-destroyer/c
  (-> response? void?))

(define status-code/c
  (integer-in 100 999))

(define (make-response status headers output history closer destroyer)
  (match status
    [(regexp #rx#"^HTTP/(...) ([1-9][0-9][0-9])(?: (.*))?$"
             (list status-line
                   http-version
                   (app bytes->number status-code)
                   status-message))
     (define-values (retaining-output retain)
       (make-retaining-input-port output))
     (define resp
       (response
        #;status-linse status-line
        #;http-version http-version
        #;status-code status-code
        #;status-message (or status-message #"")
        #;headers headers
        #;output retaining-output
        #;data #f
        #;history history
        #;close closer
        #;closed? #f
        #;destroyer destroyer))
     (retain resp)
     resp]
    [_
     (raise-argument-error 'status "a valid status line" status)]))

(define-syntax-rule (define-headers-ref id for-form)
  (begin
    (provide
     (contract-out [id (-> response? symbol? any/c)]))
    (define (id r h)
      (define h:bs (symbol->bytes h))
      (define re (byte-regexp (bytes-append #"^(?i:" (regexp-quote h:bs) #"): ")))
      (for-form ([header (in-list (response-headers r))]
                 #:when (regexp-match re header))
                (subbytes header (+ 2 (bytes-length h:bs)))))))

(define-headers-ref response-headers-ref for/first)
(define-headers-ref response-headers-ref* for/list)

(define (response-body r)
  (unless (response-data r)
    (response-drain! r))
  (response-data r))

(define (response-json r)
  (bytes->jsexpr (response-body r)))

(define (response-xexpr r)
  (xml->xexpr
   (document-element
    (response-xml r))))

(define (response-xml r)
  (read-xml/document (open-input-bytes (response-body r))))

(define (read-response r)
  (read (response-output r)))

(define (read-response-json r)
  (read-json (response-output r)))

(define (read-response-xexpr r)
  (xml->xexpr
   (document-element
    (read-response-xml r))))

(define (read-response-xml r)
  (read-xml/document (response-output r)))

(define (response-drain! r [t #f])
  (unless (response-data r)
    (parameterize-break #f
      (define drain-promise
        (delay/thread
         (with-handlers ([exn:break? void])
           (parameterize-break #t
             (define in (response-output r))
             (unless (port-closed? in)
               (define data (port->bytes in))
               (set-response-data! r data)
               (close-input-port in))))))
      (unless (sync/timeout/enable-break t drain-promise)
        (raise (make-timeout-error 'drain)))
      (force drain-promise))))

(define (response-close! r)
  ;; In order to reuse the connection, we need to drain the data port,
  ;; but draining the data port might block indefinitely, so drain with
  ;; a timeout and close the connection if the data cannot be drained in
  ;; time.
  (unless (response-closed? r)
    (parameterize-break #f
      (define drain-promise
        (delay/thread
         (define in (response-output r))
         (unless (port-closed? in)
           (copy-port in (open-output-nowhere))
           (close-input-port in))))
      (define close-thd
        (thread
         (lambda ()
           (unless (sync/timeout 1 drain-promise)
             (log-http-easy-warning "timed out while closing response")
             ((response-destroyer r) r))
           ((response-closer r) r)
           (log-http-easy-debug "response closed"))))
      (set-response-closed?! r #t)
      (sync/enable-break (thread-dead-evt close-thd))
      (force drain-promise))))


;; match expanders ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (rename-out [response:me response]))

(define-match-expander heads
  (syntax-parser
    ([_ (name:id value:expr) ... (~optional rst)]
     #:with (head-re ...)
     (for/list ([name (syntax->datum #'(name ...))])
       (datum->syntax #'name (bytes-append #"^(?i:" (regexp-quote (symbol->bytes name)) #"): (.*)")))
     #'(list-no-order (regexp head-re (list _ value)) ... (~? rst _) (... ...)))))

(define-match-expander response:me
  (syntax-parser
    ([_ (~alt (~optional (~seq #:status-line line))
              (~optional (~seq #:status-code code))
              (~optional (~seq #:status-message message))
              (~optional (~seq #:http-version version))
              (~optional (~seq #:history history))
              (~optional (~seq #:headers (headers ...) (~optional rst)))
              (~optional (~seq #:body body))
              (~optional (~seq #:json json))) ...]
     #'(? response?
          (~? (app response-status-line line))
          (~? (app response-status-code code))
          (~? (app response-status-message message))
          (~? (app response-http-version version))
          (~? (app response-history history))
          (~? (app response-headers (heads headers ... (~? rst))))
          (~? (app response-body body))
          (~? (app response-json json))))))
