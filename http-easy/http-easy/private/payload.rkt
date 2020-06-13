#lang racket/base

(require json
         net/uri-codec
         racket/contract
         "contract.rkt")

(provide
 form-payload
 json-payload)

(define/contract (form-payload v)
  (-> form-data/c payload-procedure/c)
  (define data (alist->form-urlencoded v))
  (lambda (hs)
    (values (hash-set hs 'content-type #"application/x-www-form-urlencoded; charset=utf-8") data)))

(define/contract (json-payload v)
  (-> jsexpr? payload-procedure/c)
  (define data (jsexpr->bytes v))
  (lambda (hs)
    (values (hash-set hs 'content-type #"application/json; charset=utf-8") data)))
