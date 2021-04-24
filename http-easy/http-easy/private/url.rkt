#lang racket/base

(require net/url
         racket/contract
         racket/format
         racket/string)

(provide
 urlish/c
 ->url)

(define urlish/c
  (or/c bytes? string? url?))

(define (->url urlish)
  (cond
    [(url? urlish) urlish]
    [(bytes? urlish) (string->url/dwim (bytes->string/utf-8 urlish))]
    [else (string->url/dwim urlish)]))

(define (string->url/dwim s)
  (cond
    [(regexp-match? #px"^[^:]+://" s)
     (define u (string->url s))
     (struct-copy url u
                  [scheme (string-trim #:repeat? #t (url-scheme u))]
                  [host   (string-trim #:repeat? #t (url-host   u))])]

    [(string-prefix? s "://")
     (string->url/dwim (~a "http" s))]

    [else
     (string->url/dwim (~a "http://" s))]))

(module+ internal
  (provide string->url/dwim))
