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
    "url-request-uri"

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

      (for* ([tuple (in-list tests)]
             [s (in-value (car tuple))]
             [p (in-value (cadr tuple))]
             [e (in-value (caddr tuple))])
        (check-equal? (url-request-uri (string->url* s) p) e))))

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
        (check-equal? (url->string (string->url* s)) e s))))

   (test-suite
    "url/literal"

    (test-case "roundtrips"
      (define tests
        '(("http://example.com" . "http://example.com")
          ("http://bogdan@example.com:5100" . "http://bogdan@example.com:5100")
          ("http://example.com/a/b/c" . "http://example.com/a/b/c")
          ("http://example.com/a%2Bb.mp3" . "http://example.com/a%2Bb.mp3")
          ("http://example.com/a%2Bb.mp3?c=d+e" . "http://example.com/a%2Bb.mp3?c=d%2Be")
          ("http://example.com/a%2Bb.mp3?c=d+e&f&g=h" . "http://example.com/a%2Bb.mp3?c=d%2Be&f&g=h")
          ("a/b/c" . "a/b/c")
          ("/a/b/c" . "/a/b/c")
          ("/a;b;c" . "/a;b;c")))

      (for* ([pair (in-list tests)]
             [s (in-value (car pair))]
             [e (in-value (cdr pair))])
        (check-equal? (url/literal->string (string->url/literal s)) e s)))

    (test-case "oracle"
      (define tests
        '("http://example.com"
          "http://example.com/"
          "http://example.com/a/b/c?d=e"
          "http://example.com/a/b/c?d=e f"
          "http://example.com/a;b"
          "http://example.com/a/b c;d"
          "http://example.com/a/b c;d e"
          "http://bogdan@example.com"
          "http://bogdan:secret pass@example.com"
          "http://bogdan:secret pass@example.com#fragment"
          "http://bogdan:secret pass@example.com#fragment a"
          "a/b/c"
          "/a/b/c"))

      (for ([test (in-list tests)])
        (check-equal?
         (url/literal->string (string->url/literal test))
         (url->string (string->url test))))))

   (test-suite
    "is-percent-encoded?"

    (check-false (is-percent-encoded? ""))
    (check-false (is-percent-encoded? "%"))
    (check-false (is-percent-encoded? "abc"))
    (check-false (is-percent-encoded? "a=b"))
    (check-true (is-percent-encoded? "a%2Bb"))
    (check-false (is-percent-encoded? "a%2Bb%")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests url-tests))
