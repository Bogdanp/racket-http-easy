#lang racket/base

(require file/gzip
         file/md5
         json
         net/uri-codec
         racket/contract
         racket/match
         racket/port
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


;; multipart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 multipart-payload
 (rename-out [multipart? multipart:part?]
             [make-multipart:field multipart:field]
             [make-multipart:file  multipart:file]))

(struct multipart (id) #:transparent)
(struct multipart:field multipart (content-type value) #:transparent)
(struct multipart:file multipart (content-type filename in) #:transparent)

(define stringy/c
  (or/c bytes? string?))

(define/contract (make-multipart:field id value [content-type #"text/plain"])
  (->* (stringy/c stringy/c) (stringy/c) multipart:field?)
  (multipart:field id content-type value))

(define/contract (make-multipart:file id inp [filename #"untitled"] [content-type #"application/octet-stream"])
  (->* (stringy/c input-port?) (stringy/c stringy/c) multipart:file?)
  (multipart:file id content-type filename inp))

(define/contract ((multipart-payload #:boundary [boundary #f] . fs) hs)
  (->* ()
       (#:boundary (or/c bytes? string?))
       #:rest (non-empty-listof multipart?)
       payload-procedure/c)
  (define boundary*
    (or boundary
        (format "----~a"
                (md5 (call-with-output-bytes
                      (lambda (out)
                        (display (current-inexact-milliseconds) out)))))))
  (define-values (in out)
    (make-pipe))
  (thread
   (lambda ()
     (for ([f (in-list fs)])
       (fprintf out "--~a\r\n" boundary*)
       (match f
         [(multipart:field id content-type value)
          (fprintf out "content-disposition: form-data; name=\"~a\"\r\n" id)
          (when content-type
            (fprintf out "content-type: ~a\r\n" content-type))
          (fprintf out "\r\n")
          (cond
            [(bytes? value) (display value out)]
            [(string? value) (display value out)]
            [else (copy-port value out)])
          (fprintf out "\r\n")]

         [(multipart:file id content-type filename in)
          (fprintf out "content-disposition: form-data; name=\"~a\"; filename=\"~a\"\r\n" id filename)
          (fprintf out "content-type: ~a\r\n\r\n" content-type)
          (copy-port in out)
          (fprintf out "\r\n")]))
     (fprintf out "--~a--" boundary*)
     (close-output-port out)))
  (values (hash-set hs 'content-type (format "multipart/form-data; boundary=~a" boundary*)) in))
