#lang racket/base

(provide
 bytes->number
 symbol->bytes
 method->bytes)

(define-syntax-rule (define/memo (id arg) body0 body ...)
  (define id
    (let ([h (make-hasheq)])
      (lambda (arg)
        (hash-ref! h arg (λ () body0 body ...))))))

(define bytes->number
  (compose1 string->number bytes->string/utf-8))

(define/memo (symbol->bytes s)
  (string->bytes/utf-8 (symbol->string s)))

(define/memo (method->bytes m)
  (string->bytes/utf-8 (string-upcase (symbol->string m))))
