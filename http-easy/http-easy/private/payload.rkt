#lang racket/base

(require file/gzip
         json
         net/uri-codec
         racket/contract
         "contract.rkt")

(provide
 form-payload
 gzip-payload
 json-payload
 pure-payload)

(define/contract (form-payload v)
  (-> form-data/c payload-procedure/c)
  (define data (alist->form-urlencoded v))
  (lambda (hs)
    (values (hash-set hs 'content-type #"application/x-www-form-urlencoded; charset=utf-8") data)))

(define/contract ((gzip-payload p) hs)
  (-> payload-procedure/c payload-procedure/c)
  (define-values (hs* data)
    (p hs))
  (define-values (in out)
    (make-pipe))
  (thread
   (lambda ()
     (define gzip-in
       (cond
         [(bytes? data)
          (open-input-bytes data)]
         [(string? data)
          (open-input-string data)]
         [else data]))
     (gzip-through-ports gzip-in out #f (current-seconds))
     (close-output-port out)))
  (values (hash-set hs* 'content-encoding #"gzip") in))

(define/contract (json-payload v)
  (-> jsexpr? payload-procedure/c)
  (define data (jsexpr->bytes v))
  (lambda (hs)
    (values (hash-set hs 'content-type #"application/json; charset=utf-8") data)))

(define/contract ((pure-payload v) hs)
  (-> (or/c bytes? string? input-port?) payload-procedure/c)
  (values hs v))
