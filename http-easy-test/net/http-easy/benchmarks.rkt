#lang racket/base

(require data/monocle
         json
         net/http-easy
         plot
         racket/date
         racket/format
         racket/list
         racket/match
         racket/promise
         racket/runtime-path
         rackunit
         threading
         (only-in web-server/http make-header response/full)
         "private/common.rkt")

(provide
 benchmarks)

(define-runtime-path benchmarks.json
  "benchmarks.json")

(define (~machine)
  (string->symbol
   (format
    "Racket ~a ~a (~a ~a) [~a]"
    (system-type 'vm)
    (version)
    (system-type 'os*)
    (system-type 'arch)
    (system-type 'machine))))

(define (keep lst n)
  (take lst (min (length lst) n)))

(define (read-benchmarks)
  (with-handlers ([exn:fail:filesystem? (λ (_) (hash))])
    (call-with-input-file benchmarks.json read-json)))

(define (write-benchmarks benchmarks)
  (call-with-output-file benchmarks.json
    #:exists 'truncate/replace
    (lambda (out)
      (write-json
       #:indent 2
       benchmarks out))))

(define (plot-results name)
  (define machine (~machine))
  (define benchmarks
    (~> (read-benchmarks)
        ((&opt-hash-ref* machine name))
        (or null)
        (sort > #:key (λ (ht) (hash-ref ht 'timestamp)))
        (keep 5)
        (reverse)))
  (parameterize ([plot-y-label "Time (ms)"]
                 [plot-x-label "Measurements"])
    (plot-pict
     #:title (~a name)
     (for/list ([(bench idx) (in-indexed (in-list benchmarks))])
       (define timestamp
         (hash-ref bench 'timestamp))
       (discrete-histogram
        #:label (date->string (seconds->date timestamp) #t)
        #:line-color (add1 idx)
        #:color (add1 idx)
        #:x-min (add1 idx)
        #:skip (+ (length benchmarks) 0.5)
        (list
         (list "Real" (hash-ref bench 'real-time))
         (list "CPU" (hash-ref bench 'cpu-time))
         (list "GC" (hash-ref bench 'gc-time))))))))

(define (delete-benchmark name)
  (define machine (~machine))
  (define benchmarks (read-benchmarks))
  (define &benchmark (&opt-hash-ref* machine name))
  (write-benchmarks (&benchmark benchmarks #f)))

(define-check (check-benchmark name tolerance proc)
  (define timestamp (current-seconds))
  (define machine (~machine))
  (sync (system-idle-evt))
  (collect-garbage)
  (collect-garbage)
  (define-values (_ cpu-time real-time gc-time)
    (time-apply proc null))
  (define benchmarks
    (read-benchmarks))
  (define &benchmark
    (&opt-hash-ref* machine name))
  (define existing-benchmarks
    (or (&benchmark benchmarks) null))
  (unless (null? existing-benchmarks)
    (match-define
      (hash* ['real-time old-real-time]
             ['cpu-time old-cpu-time]
             ['gc-time old-gc-time])
      (car existing-benchmarks))
    (when (or (> real-time (* old-real-time tolerance))
              (> cpu-time (* old-cpu-time tolerance))
              (> gc-time (* old-gc-time tolerance)))
      (fail-check
       (string-append
        (format "benchmark ~a failed~n" name)
        (format "  real time: ~s (was: ~s; slowdown: ~a)~n" real-time old-real-time (~slowdown real-time old-real-time))
        (format "  cpu time: ~s (was: ~s; slowdown: ~a)~n" cpu-time old-cpu-time (~slowdown cpu-time old-cpu-time))
        (format "  gc time: ~s (was: ~s; slowdown: ~a)" gc-time old-gc-time (~slowdown gc-time old-gc-time))))))
  (write-benchmarks
   (&benchmark
    benchmarks
    (cons
     (hasheq
      'timestamp timestamp
      'real-time real-time
      'cpu-time cpu-time
      'gc-time gc-time)
     (keep existing-benchmarks 49)))))

(define (~slowdown current old)
  (define % (if (zero? old) 1.0 (/ current old)))
  (~a (~r #:precision '(= 2) (* % 100)) "%"))

(define (handle-OK _req)
  (response/full
   #;code 200
   #;message #"OK"
   #;seconds (current-seconds)
   #;mime #"text/plain"
   #;headers (list (make-header #"Content-Length" #"2"))
   #;body (list #"OK")))

(define benchmarks
  (test-suite
   "http-easy"

   (test-suite
    "benchmarks"

    (test-case "sequential GETs"
      (call-with-web-server
       handle-OK
       (lambda (addr)
         (parameterize ([current-session (make-session)])
           (check-benchmark
            'sequential-GET 1.20
            (lambda ()
              (for ([_ (in-range 10000)])
                (get addr))))
           (session-close! (current-session))))))

    (test-case "concurrent GETs"
      (call-with-web-server
       handle-OK
       (lambda (addr)
         (define sema (make-semaphore 3))
         (parameterize ([current-session (make-session)])
           (check-benchmark
            'concurrent-GET 1.10
            (lambda ()
              (define promises
                (for/list ([_ (in-range 10000)])
                  (delay/thread
                   (call-with-semaphore sema
                     (lambda ()
                       (get addr))))))
              (for-each force promises)))
           (session-close! (current-session)))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests benchmarks))

(module+ main
  (require pict)
  (hc-append
   (plot-results 'sequential-GET)
   (plot-results 'concurrent-GET)))
