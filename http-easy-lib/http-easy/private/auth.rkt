#lang racket/base

(require net/base64
         racket/contract/base
         racket/port
         "contract.rkt")

(provide
 (contract-out
  [basic-auth (-> (or/c bytes? string?)
                  (or/c bytes? string?)
                  auth-procedure/c)]
  [bearer-auth (-> (or/c bytes? string?) auth-procedure/c)]))

(define (basic-auth username password)
  (define header-value
    (call-with-output-bytes
     (lambda (out)
       (define s
         (string->bytes/utf-8
          (format "~a:~a" username password)))

       (write-bytes #"Basic " out)
       (write-bytes (base64-encode s #"") out))))
  (lambda (_url headers params)
    (values (hash-set headers 'authorization header-value) params)))

(define (bearer-auth token)
  (define header-value
    (call-with-output-bytes
     (lambda (out)
       (display "Bearer " out)
       (display token out))))
  (lambda (_url headers params)
    (values (hash-set headers 'authorization header-value) params)))
