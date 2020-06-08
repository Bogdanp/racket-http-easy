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
 ((3) 0 () 0 () () (c values c (u . #"Mon, 08 Jun 2020 21:08:51 GMT")))
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
