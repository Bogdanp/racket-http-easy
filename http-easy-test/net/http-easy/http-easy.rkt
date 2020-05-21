#lang racket/base

(require net/http-easy
         net/url
         rackunit
         (only-in web-server/http
                  binding-id
                  binding:form-value
                  request-bindings/raw
                  response/output)
         "private/common.rkt")

(provide
 http-easy-tests)

(define http-easy-tests
  (test-suite
   "http-easy"

   (test-suite
    "requesters"

    (test-case "can make plain requests"
      (call-with-web-server
       (lambda (_req)
         (response/output
          (lambda (out)
            (display "hello" out))))
       (lambda ()
         (check-equal? (response-body (get "http://127.0.0.1:9911")) #"hello"))))

    (test-case "can make requests with query params"
      (call-with-web-server
       (lambda (req)
         (response/output
          (lambda (out)
            (write (for/list ([bind (request-bindings/raw req)])
                     (cons (bytes->string/utf-8 (binding-id bind))
                           (bytes->string/utf-8 (binding:form-value bind))))
                   out))))
       (lambda ()
         (check-equal? (read (response-output (get "http://127.0.0.1:9911"
                                                   #:drain? #f
                                                   #:params '((a . "1")
                                                              (a . "2")
                                                              (b . "3")))))
                       '(("a" . "1")
                         ("a" . "2")
                         ("b" . "3")))

         (check-equal? (read (response-output (get (string->url "http://127.0.0.1:9911?a=0")
                                                   #:drain? #f
                                                   #:params '((a . "1")
                                                              (a . "2")
                                                              (b . "3")))))
                       '(("a" . "0")
                         ("a" . "1")
                         ("a" . "2")
                         ("b" . "3")))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests http-easy-tests))
