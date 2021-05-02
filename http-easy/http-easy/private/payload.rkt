#lang racket/base

(require file/gzip
         file/md5
         json
         net/uri-codec
         racket/contract
         racket/format
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
  (values
   (hash-set hs* 'content-encoding #"gzip")
   (~>>
    (cond
      [(bytes? data) (open-input-bytes data)]
      [(string? data) (open-input-string data)]
      [else data])
    (lambda (in out)
      (gzip-through-ports in out #f (current-seconds))))))

(define/contract (json-payload v)
  (-> jsexpr? payload-procedure/c)
  (lambda (hs)
    (values
     (hash-set hs 'content-type #"application/json; charset=utf-8")
     (~>> v write-json))))

(define/contract ((pure-payload v) hs)
  (-> (or/c bytes? string? input-port?) payload-procedure/c)
  (values hs v))


;; multipart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 multipart-payload
 part?
 field-part
 file-part)

(struct part (id) #:transparent)
(struct part:field part (content-type value) #:transparent)
(struct part:file part (content-type filename in) #:transparent)

(define stringy/c
  (or/c bytes? string?))

(define/contract (field-part id value [content-type #"text/plain"])
  (->* (stringy/c (or/c stringy/c input-port?)) (stringy/c) part:field?)
  (part:field id content-type value))

(define/contract (file-part id inp [filename (~a (object-name inp))] [content-type #"application/octet-stream"])
  (->* (stringy/c input-port?) (stringy/c stringy/c) part:file?)
  (part:file id content-type filename inp))

(define/contract ((multipart-payload #:boundary [boundary #f] . fs) hs)
  (->* ()
       (#:boundary (or/c bytes? string?))
       #:rest (non-empty-listof part?)
       payload-procedure/c)
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
