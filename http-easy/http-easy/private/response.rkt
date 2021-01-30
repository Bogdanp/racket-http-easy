#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "common.rkt")
         json
         racket/contract
         racket/match
         racket/port
         xml
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
 response-headers-ref*
 response-history
 response-output
 response-body
 response-json
 response-xexpr
 response-xml
 response-drain!
 response-close!
 read-response
 read-response-json
 read-response-xexpr
 read-response-xml)

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

(define/contract (make-response status headers out history closer)
  (-> bytes? (listof bytes?) input-port? (listof response?) response-closer/c response?)
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
               history
               closer
               #f)]

    [_
     (raise-argument-error 'status "a valid status line" status)]))

(define-syntax-rule (define-headers-ref id for-form)
  (define/contract (id r h)
    (-> response? symbol? any/c)
    (define h:bs (symbol->bytes h))
    (define re (byte-regexp (bytes-append #"^(?i:" (regexp-quote h:bs) #"): ")))
    (for-form ([header (in-list (response-headers r))]
               #:when (regexp-match re header))
      (subbytes header (+ 2 (bytes-length h:bs))))))

(define-headers-ref response-headers-ref for/first)
(define-headers-ref response-headers-ref* for/list)

(define/contract (response-body r)
  (-> response? bytes?)
  (unless (response-data r)
    (response-drain! r))
  (response-data r))

(define/contract (response-json r)
  (-> response? (or/c eof-object? jsexpr?))
  (bytes->jsexpr (response-body r)))

(define/contract (response-xexpr r)
  (-> response? xexpr?)
  (xml->xexpr
   (document-element
    (response-xml r))))

(define/contract (response-xml r)
  (-> response? document?)
  (read-xml/document (open-input-bytes (response-body r))))

(define/contract (read-response r)
  (-> response? any/c)
  (read (response-output r)))

(define/contract (read-response-json r)
  (-> response? (or/c eof-object? jsexpr?))
  (read-json (response-output r)))

(define/contract (read-response-xexpr r)
  (-> response? xexpr?)
  (xml->xexpr
   (document-element
    (read-response-xml r))))

(define/contract (read-response-xml r)
  (-> response? document?)
  (read-xml/document (response-output r)))

(define/contract (response-drain! r)
  (-> response? void?)
  (call-with-semaphore (response-sema r)
    (lambda ()
      (unless (response-data r)
        (define inp (response-output r))
        (unless (port-closed? inp)
          (define data (port->bytes inp))
          (set-response-data! r data)
          (close-input-port inp))))))

(define/contract (response-close! r)
  (-> response? void?)
  (call-with-semaphore (response-sema r)
    (lambda ()
      (unless (response-closed? r)
        (define inp (response-output r))
        (unless (port-closed? inp)
          (copy-port inp (open-output-nowhere))
          (close-input-port inp))
        ((response-closer r) r)
        (set-response-closed?! r #t)))))


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
