#lang racket/base

(require json
         net/http-easy
         net/uri-codec
         net/url
         sentry
         sentry/tracing)

(provide
 make-tracing-middleware)

(define (make-tracing-middleware sentry)
  (make-keyword-procedure
   (lambda (kws kw-args u k . args)
     (parameterize ([current-sentry sentry])
       (define method (kw-ref kws kw-args '#:method))
       (define params (kw-ref kws kw-args '#:params))
       (define method-str (string-upcase (symbol->string method)))
       ;; https://develop.sentry.dev/sdk/telemetry/traces/span-data-conventions/
       (define data
         (hasheq
          'http.request.method
          method-str
          'http.query
          (format
           "?~a"
           (alist->form-urlencoded
            (append
             (or (url-query u) null)
             (or params null))))
          'server.address
          (or (url-host u)
              (json-null))
          'server.port
          (or (url-port u)
              (case (url-scheme u)
                [("https") 443]
                [else 80]))))
       (call-with-span
        #:origin 'auto.http.easy
        #:operation 'http.client
        #:description (format "~a ~a" method-str (url->string u))
        #:data data
        (lambda (s)
          (define res (keyword-apply k kws kw-args u args))
          (span-set! s 'http.response.status_code (response-status-code res))
          (cond
            [(response-headers-ref res 'content-length)
             => (λ (len) (span-set! s 'http.response_content_length len))]
            [(response-closed? res)
             (define len (bytes-length (response-body res)))
             (span-set! s 'http.response_content_length len)]
            [else (void)])
          res))))))

(define (kw-ref kws kw-args needle)
  (for/first ([kw (in-list kws)]
              [kw-arg (in-list kw-args)]
              #:when (equal? kw needle))
    kw-arg))
