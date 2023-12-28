#lang racket/base

(require racket/contract/base
         racket/format
         racket/lazy-require
         racket/match
         racket/port
         "contract.rkt")

(lazy-require
 [file/gzip (gzip-through-ports)]
 [file/md5 (md5)]
 [json (jsexpr? jsexpr->bytes)]
 [net/uri-codec (alist->form-urlencoded)])

(provide
 (contract-out
  [form-payload (-> form-data/c payload-procedure/c)]
  [gzip-payload (-> payload-procedure/c payload-procedure/c)]
  [json-payload (-> jsexpr? payload-procedure/c)]
  [pure-payload (-> (or/c bytes? string? input-port?) payload-procedure/c)]))

(define (form-payload v)
  (define data (alist->form-urlencoded v))
  (lambda (hs)
    (values (hash-set hs 'content-type #"application/x-www-form-urlencoded; charset=utf-8") data)))

(define ((gzip-payload p) hs)
  (define-values (hs* data)
    (p hs))
  (values
   (hash-set hs* 'content-encoding #"gzip")
   (~>>
    (cond
      [(bytes? data) (open-input-bytes data)]
      [(string? data) (open-input-string data)]
      [else data])
    (lambda (in out)
      (gzip-through-ports in out #f (current-seconds))))))

(define (json-payload v)
  (lambda (hs)
    (values
     (hash-set hs 'content-type #"application/json; charset=utf-8")
     (jsexpr->bytes v))))

(define ((pure-payload v) hs)
  (values hs v))


;; multipart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [part? (-> any/c boolean?)]
  [field-part (->* (stringy/c (or/c stringy/c input-port?)) (stringy/c) part:field?)]
  [file-part (->* (stringy/c input-port?) (stringy/c stringy/c) part:file?)]
  [multipart-payload
   (->* ()
        (#:boundary (or/c bytes? string?))
        #:rest (non-empty-listof part?)
        payload-procedure/c)]))

(struct part (id) #:transparent)
(struct part:field part (content-type value) #:transparent)
(struct part:file part (content-type filename in) #:transparent)

(define stringy/c
  (or/c bytes? string?))

(define (field-part id value [content-type #"text/plain"])
  (part:field id content-type value))

(define (file-part id inp [filename (~a (object-name inp))] [content-type #"application/octet-stream"])
  (part:file id content-type filename inp))

(define ((multipart-payload #:boundary [boundary #f] . fs) hs)
  (let ([boundary (or boundary (generate-boundary))])
    (values
     (hash-set hs 'content-type (format "multipart/form-data; boundary=~a" boundary))
     (~>> fs (make-parts-writer boundary)))))

(define ((make-parts-writer boundary) fs out)
  (for ([f (in-list fs)])
    (fprintf out "--~a\r\n" boundary)
    (match f
      [(part:field id content-type value)
       (fprintf out "content-disposition: form-data; name=\"~a\"\r\n" (quote-multipart id))
       (when content-type
         (fprintf out "content-type: ~a\r\n" content-type))
       (fprintf out "\r\n")
       (cond
         [(bytes? value) (display value out)]
         [(string? value) (display value out)]
         [else (copy-port value out)])
       (fprintf out "\r\n")]

      [(part:file id content-type filename in)
       (fprintf out "content-disposition: form-data; name=\"~a\"; filename=\"~a\"\r\n" (quote-multipart id) (quote-multipart filename))
       (fprintf out "content-type: ~a\r\n\r\n" content-type)
       (copy-port in out)
       (fprintf out "\r\n")]))
  (fprintf out "--~a--\r\n" boundary))

(define (quote-multipart name)
  (regexp-replace* #rx"[\"\\]" name "\\\\\\0"))

(define (generate-boundary)
  (with-output-to-bytes
    (lambda ()
      (display "--------http-easy-")
      (display (md5 (call-with-output-bytes
                     (lambda (out)
                       (display (current-inexact-milliseconds) out))))))))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Asynchronously write `data' to a new input port using `f'.
(define (~>> data f)
  (define-values (in out)
    (make-pipe))
  (begin0 in
    (thread
     (lambda ()
       (dynamic-wind
         void
         (lambda ()
           (f data out))
         (lambda ()
           (close-output-port out)))))))
