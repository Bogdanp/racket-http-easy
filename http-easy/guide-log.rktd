;; This file was created by make-log-based-eval
((require racket/contract) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((require net/http-easy) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define res (get "https://example.com"))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((response-status-code res) ((3) 0 () 0 () () (q values 200)) #"" #"")
((response-status-message res)
 ((3) 0 () 0 () () (c values c (u . #"OK")))
 #""
 #"")
((response-headers-ref res 'date)
 ((3) 0 () 0 () () (c values c (u . #"Tue, 09 Jun 2020 10:42:11 GMT")))
 #""
 #"")
((subbytes (response-body res) 0 30)
 ((3) 0 () 0 () () (c values c (u . #"<!doctype html>\n<html>\n<head>\n")))
 #""
 #"")
((response-close! res) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define res (get "https://example.com" #:drain? #f))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((input-port? (response-output res)) ((3) 0 () 0 () () (q values #t)) #"" #"")
((read-string 5 (response-output res))
 ((3) 0 () 0 () () (c values c (u . "<!doc")))
 #""
 #"")
((read-string 5 (response-output res))
 ((3) 0 () 0 () () (c values c (u . "type ")))
 #""
 #"")
((response-status-line
  (get "https://httpbin.org/basic-auth/Aladdin/OpenSesame"))
 ((3) 0 () 0 () () (c values c (u . #"HTTP/1.1 401 UNAUTHORIZED")))
 #""
 #"")
((response-json
  (get
   "https://httpbin.org/basic-auth/Aladdin/OpenSesame"
   #:auth
   (auth/basic "Aladdin" "OpenSesame")))
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (h - () (authenticated . #t) (user u . "Aladdin"))))
 #""
 #"")
((response-json
  (get "https://httpbin.org/bearer" #:auth (auth/bearer "secret-api-key")))
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (h - () (authenticated . #t) (token u . "secret-api-key"))))
 #""
 #"")
((response-json
  (get
   "https://httpbin.org/bearer"
   #:auth
   (lambda (uri headers params)
     (values
      (hash-set headers 'authorization "Bearer secret-api-key")
      params))))
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (h - () (authenticated . #t) (token u . "secret-api-key"))))
 #""
 #"")
