#lang racket/base

(require json
         net/cookies
         net/http-easy
         net/url
         racket/class
         racket/match
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
       (lambda (addr)
         (check-equal? (response-body (get addr)) #"hello"))))

    (test-case "can make requests with query params"
      (call-with-web-server
       (lambda (req)
         (response/output
          (lambda (out)
            (write (for/list ([bind (request-bindings/raw req)])
                     (cons (bytes->string/utf-8 (binding-id bind))
                           (bytes->string/utf-8 (binding:form-value bind))))
                   out))))
       (lambda (addr)
         (check-equal?
          (read-response
           (get addr
                #:stream? #t
                #:params '((a . "1")
                           (a . "2")
                           (b . "3"))))
          '(("a" . "1")
            ("a" . "2")
            ("b" . "3")))

         (check-equal?
          (read-response
           (get (string->url (format "~a?a=0" addr))
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
       (lambda (addr)
         (check-equal?
          (read
           (open-input-bytes
            (response-body
             (get addr #:close? #t))))
          #"close"))))

    (test-suite
     "bodies"

     (test-case "can send bodies"
       (call-with-web-server
        (lambda (req)
          (response/output
           (lambda (out)
             (display (request-post-data/raw req) out))))
        (lambda (addr)
          (check-equal? (response-body (post addr #:data #"hello")) #"hello")
          (check-equal? (response-body (post addr #:data "hello")) #"hello")
          (check-equal? (response-body (post addr #:data (open-input-string "hello"))) #"hello")))))

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
        (lambda (addr)
          (check-equal?
           (read-response
            (get addr #:stream? #t)) 'fail)
          (check-equal?
           (read-response
            (get addr
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
        (lambda (addr)
          (check-equal?
           (read-response
            (post addr
                  #:stream? #t
                  #:form '((hello . "world"))))
           '((#"hello" . #"world"))))))

     (test-case "can send json payloads"
       (call-with-web-server
        (lambda (req)
          (response/output
           (lambda (out)
             (write (bytes->jsexpr (request-post-data/raw req)) out))))
        (lambda (addr)
          (check-equal?
           (read-response
            (post addr
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
        (lambda (addr)
          (test-case "can follow redirects"
            (check-equal? (response-body (get addr)) #"hello"))

          (test-case "redirects can be exhausted"
            (check-equal?
             (response-status-code (get addr #:max-redirects 1))
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
        (lambda (addr)
          (check-equal? (response-body (post addr)) #"hello"))))

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
        (lambda (addr)
          (check-equal? (response-body (post addr)) #"hello"))))

     (test-case "redirects to other origins discard auth"
       (call-with-web-server
        (lambda (req)
          (response/output
           (lambda (out)
             (if (headers-assq* #"authorization" (request-headers/raw req))
                 (write 'fail out)
                 (write 'ok out)))))
        (lambda (addr-1)
          (define-values (dispatch _)
            (dispatch-rules
             [("")
              (lambda (_)
                (redirect-to "/a"))]

             [("a")
              (lambda (_)
                (redirect-to addr-1))]))

          (call-with-web-server
           (lambda (req)
             (match (headers-assq* #"authorization" (request-headers/raw req))
               [(header #"authorization" #"Basic QWxhZGRpbjpPcGVuU2VzYW1l")
                (dispatch req)]

               [_
                (response/output
                 (lambda (out)
                   (write 'fail/auth out)))]))
           (lambda (addr-2)
             (check-equal?
              (read-response
               (get addr-2
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
        (lambda (addr)
          (check-false (read-response (get addr #:stream? #t)))
          (check-false (read-response (get addr #:stream? #t))))))

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
        (lambda (addr)
          (parameterize ([current-session (make-session #:cookie-jar (new list-cookie-jar%))])
            (check-false (read-response (get addr #:stream? #t)))
            (check-equal? (read-response (get addr #:stream? #t))
                          #"a-cookie=hello"))))))

    (test-suite
     "timeouts"

     (test-case "raises response timeouts when the remote end is too slow"
       (define counter 0)
       (call-with-web-server
        (lambda (_req)
          (sleep 3)
          (response/output
           (lambda (out)
             (define id counter)
             (set! counter (add1 counter))
             (fprintf out "hello (~a)" id))))
        (lambda (addr)
          (check-exn
           exn:fail:http-easy:timeout?
           (lambda ()
             (get addr #:timeouts (make-timeout-config #:request 1))))
          ;; Issue #21
          (check-equal?
           (response-body (get addr))
           #"hello (1)")))))

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
        (lambda (addr)
          (parameterize ([current-session (make-session)])
            (check-equal?
             (read-response
              (post
               #:stream? #t
               #:data (multipart-payload
                       (field-part "a" (open-input-string "hello"))
                       (file-part "f" (open-input-string "{}") "a.json")
                       (file-part "f" (open-input-string "{}") "b.json"))
               addr))
             (list
              (cons #"a.json" #"{}")
              (cons #"b.json" #"{}"))))))))

    (test-suite
     "non-compliant servers"

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
           #"hello")))))

    (test-suite
     "custom port"

     ;; xref: racket-http-easy#26
     (test-case "can commit peeked progress"
       (call-with-web-server
        (lambda (_req)
          (response/output
           (lambda (out)
             (displayln "hello, world!" out))))
        (lambda (addr)
          (parameterize ([current-session (make-session)])
            (define r (get #:stream? #t addr))
            (define in (response-output r))
            (check-equal? (peek-bytes 5 0 in) #"hello")
            (check-true (port-commit-peeked 5 (port-progress-evt in) always-evt in))
            (check-equal? (read-bytes 8 in) #", world!")))))))

   (test-suite
    "session"

    (test-suite
     "breaks"

     (test-case "can break a request"
       (call-with-web-server
        (lambda (_req)
          (sleep 30)
          (response/output
           (lambda (out)
             (displayln "hello, world!" out))))
        (lambda (addr)
          (define thd
            (thread
             (lambda ()
               (with-handlers ([exn:break? void])
                 (get addr)))))
          (sync (system-idle-evt))
          (break-thread thd)
          (sync (system-idle-evt))
          (check-not-false (sync/timeout 0 thd)))))

     (test-case "breaking is safe"
       (define sema (make-semaphore))
       (call-with-web-server
        (lambda (_req)
          (response/output
           (lambda (out)
             (semaphore-wait sema)
             (displayln "hello, world!" out))))
        (lambda (addr)
          (parameterize ([current-session
                          (make-session
                           #:pool-config
                           (make-pool-config
                            #:max-size 1))])
            (define thd
              (thread
               (lambda ()
                 (with-handlers ([exn:break? void])
                   (get addr)))))
            (sync (system-idle-evt))
            (break-thread thd)
            (semaphore-post sema)
            (semaphore-post sema)
            (check-not-false (get addr))))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests http-easy-tests))
