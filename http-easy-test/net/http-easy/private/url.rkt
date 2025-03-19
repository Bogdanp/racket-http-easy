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
          ("http://example.com/a%2Bb.mp3?c=d+e" . "http://example.com/a%2Bb.mp3?c=d+e")
          ("http://example.com/a%2Bb.mp3?c=d+e&f&g=h" . "http://example.com/a%2Bb.mp3?c=d+e&f&g=h")
          ("https://yleawsaudioipv4.akamaized.net/download/world/78-e0812afa331548619c40a31f60a2d6c3/audio-1742305456621.mp3/filename/Nyhetspodden-Alla-vill-ha-Gronland--men-vad-vill-gronlanningarna-sjalva-2025-03-19.mp3?hdnts=exp=1742456407~acl=/download/world/78-e0812afa331548619c40a31f60a2d6c3/audio-1742305456621.mp3/filename/Nyhetspodden-Alla-vill-ha-Gronland--men-vad-vill-gronlanningarna-sjalva-2025-03-19.mp3~hmac=4cf1fae52a5f2aea1a12f2d677c364668a893b311a4ed761d9d7c259b229841a"
           . "https://yleawsaudioipv4.akamaized.net/download/world/78-e0812afa331548619c40a31f60a2d6c3/audio-1742305456621.mp3/filename/Nyhetspodden-Alla-vill-ha-Gronland--men-vad-vill-gronlanningarna-sjalva-2025-03-19.mp3?hdnts=exp=1742456407~acl=/download/world/78-e0812afa331548619c40a31f60a2d6c3/audio-1742305456621.mp3/filename/Nyhetspodden-Alla-vill-ha-Gronland--men-vad-vill-gronlanningarna-sjalva-2025-03-19.mp3~hmac=4cf1fae52a5f2aea1a12f2d677c364668a893b311a4ed761d9d7c259b229841a")
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
         (url->string (string->url test)))))

    (test-case "examples"
      (let ([example "https://d12xz7rzfw7xh7.cloudfront.net/v1/download/episodes/original/43796816?a=en&eg=https%3A%2F%2Fapi.spreaker.com%2Fepisode%2F57758598&eu=https%3A%2F%2Fdts.podtrac.com%2Fredirect.mp3%2Fapi.spreaker.com%2Fdownload%2Fepisode%2F57758598%2Ftmp9u92imeh.mp3&p=3&q=9808638&f=559&r=128&t=3&u=11393707&o=2401044&d=2023-11-22&g=57758598&h=5937276&k=https%3A%2F%2Fwww.spreaker.com%2Fshow%2F5937276%2Fepisodes%2Ffeed&i=43796816&n=Petros+And+Money&b=%5B%22IAB6-7%22%2C%22IAB7-39%22%2C%22IAB11-4%22%2C%22IAB26%22%5D&c=%5B%22sports%22%5D&l=%5B%22hosting_plan_ihr%22%5D&m=%5B904294%2C904294%2C904294%2C1436858%2C1436858%2C1436858%2C1436858%2C1436858%2C1937091%2C1937091%2C1937091%5D&rr=4444444444444&fax=0.4&Expires=1732993926&Signature=XDgffPCg91Gd6ThNXoenP4axeBN2zEUK6Bs56F2Pw-LGE9XJuLPghg1f2etV1l6I3%7Ed7Ms12AQbkCp1vfkqleStA30fPDH2PpO1IKkw5k7PlSYyPCeb1DOc1No8s6KHn7C8DZ7swXjWEz5WGzrj6KtSgI%7EWMhQyiLuGxEmT9YBQViowMGeO7p1PNocQmT-SKo8WqMDMdzMmSXP2WQFYSk3AjFM2ukhGLzDkIcrNxy2ZRLGeUykF9ZWgNnGGAOfwsmx6n0IFQZcdDo2QpRKxOUjSBYOTxTo1Y716OYb73P59QurF%7El-jM7WLHgvFxWnJQHj9SwCnjSaCJgzDBlh%7EEDQ__&Key-Pair-Id=K1J2BR3INU6RYD"])
        (check-equal? (url/literal->string (string->url/literal example)) example))))

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
