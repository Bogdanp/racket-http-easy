#lang info

(define collection "tests")
(define deps '("base"))
(define build-deps '("http-easy"
                     "net-cookies-lib"
                     "rackunit-lib"
                     "resource-pool-lib"
                     "web-server-lib"))
(define update-implies '("http-easy"))
