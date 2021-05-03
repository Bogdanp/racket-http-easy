#lang info

(define version "0.3.2")
(define collection "net")
(define deps '("base"
               "memoize"
               "net-cookies-lib"
               ("resource-pool-lib" #:version "0.1")
               "unix-socket-lib"))
(define build-deps '("net-cookies-doc"
                     "net-doc"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))
(define scribblings '(("http-easy.scrbl")))
