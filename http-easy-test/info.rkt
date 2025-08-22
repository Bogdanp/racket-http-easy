#lang info

(define license 'BSD-3-Clause)
(define collection "tests")
(define deps
  '("base"))
(define build-deps
  '("http-easy"
    "monocle-lib"
    "net-cookies-lib"
    "pict-lib"
    "plot-gui-lib"
    "plot-lib"
    "rackunit-lib"
    ["resource-pool-lib" #:version "0.1"]
    "threading-lib"
    "web-server-lib"))
(define update-implies
  '("http-easy"))
