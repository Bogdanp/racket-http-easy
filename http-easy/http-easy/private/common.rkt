#lang racket/base

(require memoize
         net/url
         racket/string)

(provide
 bytes->number
 symbol->bytes
 method->bytes
 url-path-string)

(define bytes->number
  (compose1 string->number bytes->string/utf-8))

(define/memo (symbol->bytes s)
  (string->bytes/utf-8 (symbol->string s)))

(define/memo (method->bytes m)
  (string->bytes/utf-8 (string-upcase (symbol->string m))))

(define (url-path-string u)
  (string-append "/" (string-join (map path/param-path (url-path u)) "/")))
