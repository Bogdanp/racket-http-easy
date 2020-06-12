#lang racket/base

(require net/url
         racket/contract)

(provide
 method/c
 headers/c
 query-params/c
 auth-procedure/c)

(define method/c
  (or/c 'delete 'head 'get 'options 'patch 'post 'put symbol?))

(define headers/c
  (hash/c symbol? (or/c bytes? string?)))

(define query-params/c
  (listof (cons/c symbol? (or/c false/c string?))))

(define auth-procedure/c
  (-> url? headers/c query-params/c (values headers/c query-params/c)))