#lang racket/base

(require net/http-client
         net/url
         openssl
         racket/contract
         "url.rkt")

(provide
 proxy?
 make-proxy
 proxy-matches?
 proxy-connect!

 make-http-proxy
 make-https-proxy)

(struct proxy (matches? connect!)
  #:transparent)

(define/contract (make-proxy matches? connect!)
  (-> (-> url? boolean?) (-> http-conn? url? (or/c #f ssl-client-context?) void?) proxy?)
  (proxy matches? connect!))

(define/contract (make-http-proxy urlish [matches? (λ (u) (equal? (url-scheme u) "http"))])
  (->* (urlish/c) ((-> url? boolean?)) proxy?)
  (proxy matches? (make-proxy-connector urlish 80 (λ (_) #f))))

(define/contract (make-https-proxy urlish [matches? (λ (u) (equal? (url-scheme u) "https"))])
  (->* (urlish/c) ((-> url? boolean?)) proxy?)
  (proxy matches? (make-proxy-connector urlish 443)))

(define (make-proxy-connector urlish default-port [ssl-ctx-f values])
  (define proxy-url (->url urlish))
  (define proxy-host (url-host proxy-url))
  (define proxy-port (or (url-port proxy-url) default-port))
  (lambda (conn u ssl-ctx)
    (define target-host (url-host u))
    (define target-port (or (url-port u) default-port))
    (define-values (ssl-ctx* in out abandon!)
      (http-conn-CONNECT-tunnel proxy-host proxy-port target-host target-port #:ssl? (ssl-ctx-f ssl-ctx)))
    (http-conn-open! conn target-host
                     #:port target-port
                     #:ssl? (list ssl-ctx* in out abandon!))))
