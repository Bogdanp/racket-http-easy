#lang info

(define collection "net")
(define deps
  '("base"
    "http-easy-lib"))
(define build-deps
  '("net-cookies-doc"
    "net-cookies-lib"
    "net-doc"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))
(define implies
  '("http-easy-lib"))
(define scribblings
  '(("http-easy.scrbl")))
