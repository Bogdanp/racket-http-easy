#lang racket/base

(require net/uri-codec
         net/url
         racket/contract/base
         racket/match
         racket/serialize
         racket/string)

(provide
 (struct-out url/literal)
 string->url/literal
 url/literal->string
 is-percent-encoded?
 urlish/c
 ->url
 url-scheme*
 url-port*
 url-request-uri)

;; A url/literal is like a url from net/url, but the user, path, query
;; and fragment are not decoded upon conversion from string and any
;; components that are already percent-encoded within those fields are
;; skipped upon conversion back to string. A component is considered to
;; already be percent-encoded if all of its % characters are followed by
;; two hexadecimal characters.
;;
;; xref https://github.com/rmculpepper/racket-http123/issues/6
(serializable-struct url/literal url ())

(define (string->url/literal s)
  (match-define (list _ scheme user ipv6host host port path query fragment)
    (regexp-match url-regexp s))
  (let* ([scheme (and scheme (string-downcase scheme))]
         [host (or (and ipv6host (string-downcase ipv6host))
                   (and host (string-downcase host)))]
         [port (and port (string->number port))]
         [abs? (or (equal? "file" scheme)
                   (regexp-match? #rx"^/" path)
                   (and (or host user port) #t))]
         [path (let ([components (regexp-split #rx"/" path)])
                 (for/list ([component (in-list (if (equal? (car components) "")
                                                    (cdr components)
                                                    components))])
                   (match-define (cons path-component params)
                     (regexp-split #rx";" component))
                   (path/param
                    (case path-component
                      [(".") 'same]
                      [("..") 'up]
                      [else path-component])
                    params)))]
         [query (if query
                    (for/list ([component (in-list (regexp-split #rx"&" query))])
                      (match (regexp-split #rx"=" component)
                        [(list name value)
                         (cons (string->symbol name) value)]
                        [(list name)
                         (cons (string->symbol name) #f)]))
                    null)])
    (url/literal scheme user host port abs? path query fragment)))

(define (url/literal->string u)
  (define out (open-output-string))
  (match-define (url scheme user host port abs? path query fragment) u)
  (when scheme
    (write-string scheme out)
    (write-char #\: out))
  (cond
    [(or user host port)
     (write-string "//" out)
     (when user
       (write-string (maybe-percent-encode user uri-userinfo-encode) out)
       (write-char #\@ out))
     (when host
       (cond
         [(ipv6-host? host)
          (write-char #\[ out)
          (write-string host out)
          (write-char #\] out)]
         [else
          (write-string host out)]))
     (when port
       (write-char #\: out)
       (display port out))]
    [(equal? scheme "file")
     (write-string "//" out)]
    [else
     (void)])
  (unless (null? path)
    (when abs? (write-char #\/ out))
    (let loop ([path-components path])
      (match-define (path/param path-component params)
        (car path-components))
      (write-string
       (maybe-percent-encode
        (case path-component
          [(same) "."]
          [(up) ".."]
          [else path-component])
        uri-path-segment-encode)
       out)
      (for ([param (in-list params)])
        (write-char #\; out)
        (write-string (maybe-percent-encode param uri-path-segment-encode) out))
      (unless (null? (cdr path-components))
        (write-char #\/ out)
        (loop (cdr path-components)))))
  (unless (null? query)
    (write-char #\? out)
    (for ([(pair idx) (in-indexed (in-list query))])
      (unless (zero? idx)
        (write-char #\& out))
      (match-define (cons (app symbol->string name) value) pair)
      (write-string (maybe-percent-encode name form-urlencoded-encode) out)
      (when value
        (write-char #\= out)
        (write-string (maybe-percent-encode value form-urlencoded-encode) out))))
  (when fragment
    (write-char #\# out)
    (write-string (maybe-percent-encode fragment) out))
  (get-output-string out))

(define (maybe-percent-encode s [encode uri-encode])
  (if (is-percent-encoded? s encode) s (encode s)))

(define (is-percent-encoded? s [encode uri-encode])
  (define num-%-matches (length (regexp-match* #rx"%" s)))
  (or (and (> num-%-matches 0)
           (= num-%-matches (length (regexp-match* #px"%[a-fA-F0-9]{2}" s))))
      (and (eq? encode form-urlencoded-encode)
           (regexp-match? #rx"[+]" s))))

(define (ipv6-host? s)
  (regexp-match? #rx"^[0-9a-fA-F:]*:[0-9a-fA-F:]*$" s))

(define urlish/c
  (or/c bytes? string? url?))

(define (->url urlish)
  (cond
    [(url? urlish) urlish]
    [(bytes? urlish) (string->url* (bytes->string/utf-8 urlish))]
    [else (string->url* urlish)]))

(define (string->url* s)
  (cond
    [(regexp-match? #px"^[^:]+://" s)
     (define the-url
       (parameterize ([current-alist-separator-mode 'amp])
         (string->url s)))
     (struct-copy
      url the-url
      [scheme (string-trim #:repeat? #t (url-scheme the-url))]
      [host   (string-trim #:repeat? #t (url-host   the-url))])]
    [(string-prefix? s "://")
     (string->url* (string-append "http" s))]
    [else
     (string->url* (string-append "http://" s))]))

(module+ internal
  (provide string->url*))

(define (url-scheme* u)
  (or (url-scheme u)
      (case (url-port u)
        [(443) "https"]
        [else  "http"])))

(define (url-port* u)
  (or (url-port u)
      (case (url-scheme u)
        [("https") 443]
        [else      80])))

(define (url-request-uri u [params null])
  (define abs-path
    (if (null? (url-path u))
        (list (path/param "" null))
        (url-path u)))
  (define all-params
    (append (url-query u) params))
  ((if (url/literal? u)
       url/literal->string
       url->string)
   (url #f #f #f #f #t abs-path all-params #f)))
