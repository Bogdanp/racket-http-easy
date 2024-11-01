#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre
                     "common.rkt")
         racket/contract/base
         racket/lazy-require
         racket/match
         racket/port
         "common.rkt"
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
  [make-response (-> bytes? (listof bytes?) input-port? (listof response?) response-closer/c response?)]
  [response-body (-> response? bytes?)]
  [response-json (-> response? (or/c eof-object? jsexpr?))]
  [response-xexpr (-> response? xexpr?)]
  [response-xml (-> response? document?)]
  [read-response (-> response? any/c)]
  [read-response-json (-> response? (or/c eof-object? jsexpr?))]
  [read-response-xexpr (-> response? xexpr?)]
  [read-response-xml (-> response? document?)]
  [response-drain! (-> response? void?)]
  [response-close! (-> response? void?)]))

(struct response
  (sema
   status-line
   http-version
   status-code
   status-message
   headers
   output
   [data #:mutable]
   history
   closer
   [closed? #:mutable]))

(define response-closer/c
  (-> response? void?))

(define status-code/c
  (integer-in 100 999))

(define (make-response status headers output history closer)
  (match status
    [(regexp #rx#"^HTTP/(...) ([1-9][0-9][0-9])(?: (.*))?$"
             (list status-line
                   http-version
                   (app bytes->number status-code)
                   status-message))
     (define-values (retaining-output retain)
       (make-retaining-input-port output))
     (define the-resp
       (response (make-semaphore 1)
                 status-line
                 http-version
                 status-code
                 (or status-message #"")
                 headers
                 retaining-output
                 #f
                 history
                 closer
                 #f))
     (begin0 the-resp
       (retain the-resp))]

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

(define (response-drain! r)
  (call-with-semaphore (response-sema r)
    (lambda ()
      (unless (response-data r)
        (define inp (response-output r))
        (unless (port-closed? inp)
          (define data (port->bytes inp))
          (set-response-data! r data)
          (close-input-port inp))))))

(define (response-close! r)
  (call-with-semaphore (response-sema r)
    (lambda ()
      (unless (response-closed? r)
        (define inp (response-output r))
        (unless (port-closed? inp)
          (copy-port inp (open-output-nowhere))
          (close-input-port inp))
        ((response-closer r) r)
        (set-response-closed?! r #t)
        (log-http-easy-debug "response closed")))))


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
