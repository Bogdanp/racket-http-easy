#lang racket/base

(require file/gunzip
         json
         net/http-easy/private/payload
         racket/port
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
      (check-equal? (read-json in) (hasheq 'hello "world"))))

   (test-suite
    "multipart-payload"

    (test-case "payload"
      (define-values (hs inp)
        ((multipart-payload
          #:boundary "the-boundary"
          (field-part "a" "hello")
          (file-part "f" (open-input-string "untitled"))
          (file-part "f" (open-input-string "hello") "hello.txt")
          (file-part "f" (open-input-string "{}") "hello.json" "application/json"))
         (hasheq)))

      (check-equal?
       hs
       (hasheq 'content-type "multipart/form-data; boundary=the-boundary"))

      (define lines
        (port->lines inp #:line-mode 'return-linefeed))
      (check-equal?
       lines
       '("--the-boundary"
         "content-disposition: form-data; name=\"a\""
         "content-type: text/plain"
         ""
         "hello"
         "--the-boundary"
         "content-disposition: form-data; name=\"f\"; filename=\"untitled\""
         "content-type: application/octet-stream"
         ""
         "untitled"
         "--the-boundary"
         "content-disposition: form-data; name=\"f\"; filename=\"hello.txt\""
         "content-type: application/octet-stream"
         ""
         "hello"
         "--the-boundary"
         "content-disposition: form-data; name=\"f\"; filename=\"hello.json\""
         "content-type: application/json"
         ""
         "{}"
         "--the-boundary--"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests payload-tests))
