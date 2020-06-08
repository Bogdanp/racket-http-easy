#lang racket/base

(require net/http-easy
         net/url
         rackunit
         web-server/dispatch
         (only-in web-server/http
                  binding-id
                  binding:form-value
                  permanently
                  redirect-to
                  request-bindings/raw
                  response/output
                  see-other
                  temporarily/same-method)
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
                         ("b" . "3"))))))

    (test-case "30[12] redirects"
      (define-values (dispatch _)
        (dispatch-rules
         [("")
          (lambda (_)
            (redirect-to "/a" permanently))]

         [("a")
          (lambda (_)
            (redirect-to "/b"))]

         [("b")
          (lambda (_)
            (response/output
             (lambda (out)
               (display "hello" out))))]))

      (call-with-web-server
       dispatch

       (lambda ()
         (test-case "can follow redirects"
           (check-equal? (response-body (get "http://127.0.0.1:9911")) #"hello"))

         (test-case "redirects can be exhausted"
           (check-equal? (response-status-code (get "http://127.0.0.1:9911"
                                                    #:max-redirects 1))
                         302)))))

    (test-case "303 redirects change the request method to GET"
      (define-values (dispatch _)
        (dispatch-rules
         [("")
          #:method "post"
          (lambda (_)
            (redirect-to "/a" see-other))]

         [("a")
          #:method "get"
          (lambda (_)
            (response/output
             (lambda (out)
               (display "hello" out))))]))

      (call-with-web-server
       dispatch
       (lambda ()
         (check-equal? (response-body (post "http://127.0.0.1:9911")) #"hello"))))

    (test-case "307 redirects preserve the request method"
      (define-values (dispatch _)
        (dispatch-rules
         [("")
          #:method "post"
          (lambda (_)
            (redirect-to "/a" temporarily/same-method))]

         [("a")
          #:method "post"
          (lambda (_)
            (response/output
             (lambda (out)
               (display "hello" out))))]))

      (call-with-web-server
       dispatch
       (lambda ()
         (check-equal? (response-body (post "http://127.0.0.1:9911")) #"hello")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests http-easy-tests))
