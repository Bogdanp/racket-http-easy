#lang racket/base

(require net/http-easy/private/url
         (submod net/http-easy/private/url internal)
         net/url
         rackunit)

(provide
 url-tests)

(define url-tests
  (test-suite
   "url"

   (test-suite
    "url-path&query"

    (test-case "extracts various kinds of paths"
      (define tests
        '(("http://example.com"    ()          "/")
          ("http://example.com"    ((a . "b")) "/?a=b")
          ("http://example.com/"   ()          "/")
          ("://example.com/a/b/c"  ()          "/a/b/c")
          ("/a/b/c/d"              ()          "/a/b/c/d")
          ("/a/b/c/d/"             ()          "/a/b/c/d/")
          ("/a/b?c=d"              ()          "/a/b?c=d")
          ("/a/b?c=å"              ()          "/a/b?c=%C3%A5")
          ("/a/b?c=å"              ((d . "e")) "/a/b?c=%C3%A5&d=e")
          ("/a;b/c"                ()          "/a;b/c")
          ("/å/b/c"                ()          "/%C3%A5/b/c")))

      (for* ([pair (in-list tests)]
             [s (in-value (car pair))]
             [p (in-value (cadr pair))]
             [e (in-value (caddr pair))])
        (check-equal? (url-path&query (string->url* s) p) e))))

   (test-suite
    "string->url*"

    (test-case "normalizes various types of URIs before parsing"
      (define tests
        '(("example.com"                   . "http://example.com")
          ("example.com:80"                . "http://example.com:80")
          ("example.com:443"               . "http://example.com:443")
          ("example.com "                  . "http://example.com")
          ("example.com/a/b/c"             . "http://example.com/a/b/c")
          ("://example.com   "             . "http://example.com")
          ("https://example.com?a=hello "  . "https://example.com?a=hello+")
          ("https://example.com "          . "https://example.com")
          ("https://example.com/a/b/c"     . "https://example.com/a/b/c")
          ("https://example.com/a/b?c=d;e" . "https://example.com/a/b?c=d%3Be")))

      (for* ([pair (in-list tests)]
             [s (in-value (car pair))]
             [e (in-value (cdr pair))])
        (check-equal? (url->string (string->url* s)) e s))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests url-tests))
