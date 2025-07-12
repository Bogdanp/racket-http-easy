#lang racket/base

(require net/http-easy/private/response
         racket/match
         rackunit)

(provide
 response-tests)

(define (make-test-response
         #:status [status #"HTTP/1.1 200 OK"]
         #:headers [headers null]
         #:body [body (open-input-bytes #"")]
         #:history [history null]
         #:closer [closer void]
         #:destroyer [destroyer void])
  (make-response status headers body history closer destroyer))

(define response-tests
  (test-suite
   "response"

   (test-suite
    "match expanders"

    (test-case "(response) matches every response"
      (check-true
       (match (make-test-response)
         [(response) #t])))

    (test-case "(response #:accessor ...) matches on #:accessor"
      (check-equal?
       (match (make-test-response)
         [(response #:status-code 400) 400]
         [(response #:status-code 200 #:http-version #"1.1") 200])
       200))

    (test-case "(response #:headers ...) matches on headers"
      (check-equal?
       (match (make-test-response #:headers (list #"Content-Type: text/html"
                                                  #"Set-Cookie: a=1"
                                                  #"Set-Cookie: b=2"))
         [(response #:headers [(set-cookie v1)
                               (set-cookie v2)] hs)
          (check-equal? hs '(#"Content-Type: text/html"))
          (bytes-append v1 v2)]

         [_ 'fail])
       #"a=1b=2")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests response-tests))
