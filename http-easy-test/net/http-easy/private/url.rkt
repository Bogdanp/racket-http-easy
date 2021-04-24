#lang racket/base

(require (submod net/http-easy/private/url internal)
         net/url
         rackunit)

(provide
 url-tests)

(define url-tests
  (test-suite
   "url"

   (test-suite
    "string->url/dwim"

    (test-case "normalizes various types of URIs before parsing"
      (define tests
        '(("example.com"                  . "http://example.com")
          ("example.com:80"               . "http://example.com:80")
          ("example.com:443"              . "http://example.com:443")
          ("example.com "                 . "http://example.com")
          ("example.com/a/b/c"            . "http://example.com/a/b/c")
          ("://example.com   "            . "http://example.com")
          ("https://example.com?a=hello " . "https://example.com?a=hello+")
          ("https://example.com "         . "https://example.com")
          ("https://example.com/a/b/c"    . "https://example.com/a/b/c")))

      (for* ([pair (in-list tests)]
             [s (in-value (car pair))]
             [e (in-value (cdr pair))])
        (check-equal? (url->string (string->url/dwim s)) e s))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests url-tests))
