#lang racket/base

(require file/gunzip
         json
         net/http-easy/private/payload
         rackunit)

(provide
 payload-tests)

(define payload-tests
  (test-suite
   "payload"

   (test-suite
    "gzip-payload"

    (test-case "roundtrip"
      (define v (hasheq 'hello "world"))
      (define-values (hs inp)
        ((gzip-payload (json-payload v)) (hasheq)))

      (check-equal? hs (hasheq 'content-encoding #"gzip"
                               'content-type #"application/json; charset=utf-8"))

      (define-values (in out) (make-pipe))
      (gunzip-through-ports inp out)
      (check-equal? (read-json in) (hasheq 'hello "world"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests payload-tests))
