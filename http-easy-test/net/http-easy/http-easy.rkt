#lang racket/base

(require json
         net/cookies
         net/http-easy
         net/url
         racket/class
         racket/match
         racket/tcp
         rackunit
         web-server/dispatch
         (only-in web-server/http
                  binding-id
                  binding:form-value
                  binding:file-filename
                  binding:file-content
                  bindings-assq
                  bindings-assq-all
                  header
                  header-value
                  headers-assq*
                  make-header
                  permanently
                  redirect-to
                  request-bindings/raw
                  request-headers/raw
                  request-post-data/raw
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
         (check-equal? (read-response (get "http://127.0.0.1:9911"
                                           #:stream? #t
                                           #:params '((a . "1")
                                                      (a . "2")
                                                      (b . "3"))))
                       '(("a" . "1")
                         ("a" . "2")
                         ("b" . "3")))

         (check-equal? (read-response (get (string->url "http://127.0.0.1:9911?a=0")
                                           #:stream? #t
                                           #:params '((a . "1")
                                                      (a . "2")
                                                      (b . "3"))))
                       '(("a" . "0")
                         ("a" . "1")
                         ("a" . "2")
                         ("b" . "3"))))))

    (test-case "#:close? sends 'Connection: close' header"
      (call-with-web-server
       (lambda (req)
         (response/output
          (lambda (out)
            (write (header-value (headers-assq* #"connection" (request-headers/raw req))) out))))
       (lambda ()
         (check-equal? (read (open-input-bytes (response-body (get "http://127.0.0.1:9911" #:close? #t))))
                       #"close"))))

    (test-suite
     "bodies"

     (test-case "can send bodies"
       (call-with-web-server
        (lambda (req)
          (response/output
           (lambda (out)
             (display (request-post-data/raw req) out))))
        (lambda ()
          (check-equal? (response-body (post "http://127.0.0.1:9911" #:data #"hello")) #"hello")
          (check-equal? (response-body (post "http://127.0.0.1:9911" #:data "hello")) #"hello")
          (check-equal? (response-body (post "http://127.0.0.1:9911" #:data (open-input-string "hello"))) #"hello")))))

    (test-suite
     "auth"

     (test-case "can authenticate requests"
       (call-with-web-server
        (lambda (req)
          (response/output
           (lambda (out)
             (match (headers-assq* #"authorization" (request-headers/raw req))
               [(header #"authorization" #"Basic QWxhZGRpbjpPcGVuU2VzYW1l")
                (write 'ok out)]

               [_
                (write 'fail out)]))))
        (lambda ()
          (check-equal? (read-response (get "http://127.0.0.1:9911" #:stream? #t)) 'fail)
          (check-equal? (read-response (get "http://127.0.0.1:9911"
                                            #:stream? #t
                                            #:auth (basic-auth "Aladdin" "OpenSesame")))
                        'ok)))))

    (test-suite
     "json"

     (test-case "can send form payloads"
       (call-with-web-server
        (lambda (req)
          (response/output
           (lambda (out)
             (write (for/list ([bind (in-list (request-bindings/raw req))])
                      (cons (binding-id bind) (binding:form-value bind)))
                    out))))
        (lambda ()
          (check-equal? (read-response (post "http://127.0.0.1:9911"
                                             #:stream? #t
                                             #:form '((hello . "world"))))
                        '((#"hello" . #"world"))))))

     (test-case "can send json payloads"
       (call-with-web-server
        (lambda (req)
          (response/output
           (lambda (out)
             (write (bytes->jsexpr (request-post-data/raw req)) out))))
        (lambda ()
          (check-equal? (read-response (post "http://127.0.0.1:9911"
                                             #:stream? #t
                                             #:json (hasheq 'hello "world")))
                        (hasheq 'hello "world"))))))

    (test-suite
     "redirects"

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
          (check-equal? (response-body (post "http://127.0.0.1:9911")) #"hello"))))

     (test-case "redirects to other origins discard auth"
       (define-values (dispatch _)
         (dispatch-rules
          [("")
           (lambda (_)
             (redirect-to "/a"))]

          [("a")
           (lambda (_)
             (redirect-to "http://127.0.0.1:9912"))]))

       (call-with-web-server
        #:port 9912
        (lambda (req)
          (response/output
           (lambda (out)
             (if (headers-assq* #"authorization" (request-headers/raw req))
                 (write 'fail out)
                 (write 'ok out)))))
        (lambda ()
          (call-with-web-server
           (lambda (req)
             (match (headers-assq* #"authorization" (request-headers/raw req))
               [(header #"authorization" #"Basic QWxhZGRpbjpPcGVuU2VzYW1l")
                (dispatch req)]

               [_
                (response/output
                 (lambda (out)
                   (write 'fail/auth out)))]))
           (lambda ()
             (check-equal? (read-response (get "http://127.0.0.1:9911"
                                               #:stream? #t
                                               #:auth (basic-auth "Aladdin" "OpenSesame")))
                           'ok)))))))

    (test-suite
     "cookies"

     (test-case "cookies are discarded by default"
       (call-with-web-server
        (lambda (req)
          (response/output
           #:headers (list (make-header #"set-cookie" (cookie->set-cookie-header
                                                       (make-cookie "a-cookie" "hello"))))
           (lambda (out)
             (write (headers-assq* #"cookie" (request-headers/raw req)) out))))
        (lambda ()
          (check-false (read-response (get "http://127.0.0.1:9911" #:stream? #t)))
          (check-false (read-response (get "http://127.0.0.1:9911" #:stream? #t))))))

     (test-case "cookie jars preserve cookies"
       (call-with-web-server
        (lambda (req)
          (response/output
           #:headers (list (make-header #"set-cookie" (cookie->set-cookie-header
                                                       (make-cookie "a-cookie" "hello"))))
           (lambda (out)
             (cond
               [(headers-assq* #"cookie" (request-headers/raw req))
                => (lambda (hdr)
                     (write (header-value hdr) out))]

               [else (write #f out)]))))
        (lambda ()
          (parameterize ([current-session (make-session #:cookie-jar (new list-cookie-jar%))])
            (check-false (read-response (get "http://127.0.0.1:9911" #:stream? #t)))
            (check-equal? (read-response (get "http://127.0.0.1:9911" #:stream? #t))
                          #"a-cookie=hello"))))))

    (test-suite
     "timeouts"

     (test-case "raises response timeouts when the remote end is too slow"
       (call-with-web-server
        (lambda (_req)
          (sleep 3)
          (response/output
           (lambda (out)
             (display "hello" out))))
        (lambda ()
          (check-exn
           exn:fail:http-easy:timeout?
           (lambda ()
             (get "http://127.0.0.1:9911"
                  #:timeouts (make-timeout-config #:request 1))))))))

    (test-suite
     "multipart payloads"

     (test-case "uploads files"
       (call-with-web-server
        (lambda (req)
          (response/output
           (lambda (out)
             (write
              (for/list ([f (in-list (bindings-assq-all #"f" (request-bindings/raw req)))])
                (cons (binding:file-filename f)
                      (binding:file-content f)))
              out))))
        (lambda ()
          (parameterize ([current-session (make-session)])
            (check-equal?
             (read-response
              (post
               #:stream? #t
               #:data (multipart-payload
                       (field-part "a" (open-input-string "hello"))
                       (file-part "f" (open-input-string "{}") "a.json")
                       (file-part "f" (open-input-string "{}") "b.json"))
               "http://127.0.0.1:9911"))
             (list
              (cons #"a.json" #"{}")
              (cons #"b.json" #"{}"))))))))

    (test-suite
     "handle non-compliant servers"

     ;; xref: racket-http-easy#18
     (test-case "response without status reason"
       (call-with-tcp-server
        (lambda (_lines out)
          (fprintf out "HTTP/1.1 200\r\n")
          (fprintf out "Connection: close\r\n")
          (fprintf out "Content-Length: 5\r\n")
          (fprintf out "\r\n")
          (fprintf out "hello"))
        (lambda (port)
          (check-equal?
           (response-body
            (get (format "http://127.0.0.1:~a" port)))
           #"hello"))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests http-easy-tests))
