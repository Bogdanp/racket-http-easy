#lang info

(define version "0.0.0")
(define collection "net")
(define deps '("base"
               "memoize"))
(define build-deps '("net-doc"
                     "racket-doc"
                     "scribble-lib"))
(define scribblings '(("http-easy.scrbl")))
