#lang info

(define collection "tests")
(define deps '("base"))
(define build-deps '("http-easy"
                     "net-cookies-lib"
                     "rackunit-lib"
                     ("resource-pool-lib" #:version "0.1")
                     "web-server-lib"))
(define update-implies '("http-easy"))
