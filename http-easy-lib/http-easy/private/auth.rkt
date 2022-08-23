#lang racket/base

(require net/base64
         racket/contract
         racket/port
         "contract.rkt")

(provide
 basic-auth
 bearer-auth)

(define/contract (basic-auth username password)
  (-> (or/c bytes? string?)
      (or/c bytes? string?)
      auth-procedure/c)
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

(define/contract (bearer-auth token)
  (-> (or/c bytes? string?) auth-procedure/c)
  (define header-value
    (call-with-output-bytes
     (lambda (out)
       (display "Bearer " out)
       (display token out))))
  (lambda (_url headers params)
    (values (hash-set headers 'authorization header-value) params)))
