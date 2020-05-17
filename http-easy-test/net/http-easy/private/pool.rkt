#lang racket/base

(require net/http-client
         net/http-easy
         net/http-easy/private/pool
         rackunit)

(provide
 pool-tests)

(define pool-tests
  (test-suite
   "pool"

   (test-suite
    "pool-lease"

    (test-case "leases connections from the pool"
      (define p (make-pool (make-pool-config #:max-size 1) values))
      (define c (pool-lease p))
      (check-true (http-conn? c))
      (pool-release p c))

    (test-case "leases connections from the pool to waiters"
      (define p (make-pool (make-pool-config #:max-size 1) values))
      (define c1 (pool-lease p))
      (define c2 #f)
      (define thd
        (thread
         (lambda ()
           (set! c2 (pool-lease p)))))

      (pool-release p c1)
      (sync thd)
      (check-true (http-conn? c2)))

    (test-case "times out when no connections take too long"
      (define p
        (make-pool (make-pool-config #:max-size 1)
                   (lambda (c)
                     (begin0 c
                       (sleep 10)))))

      (check-exn
       (lambda (e)
         (and (exn:fail:http-easy:timeout? e)
              (eq? (exn:fail:http-easy:timeout-kind e) 'connect)))
       (lambda ()
         (pool-lease p (make-timeout-config #:connect 0.01)))))

    (test-case "times out when no connections are available"
      (define p (make-pool (make-pool-config #:max-size 1) values))
      (define c1 (pool-lease p))
      (check-exn
       (lambda (e)
         (and (exn:fail:http-easy:timeout? e)
              (eq? (exn:fail:http-easy:timeout-kind e) 'lease)))
       (lambda ()
         (pool-lease p (make-timeout-config #:lease 0.01))))

      (pool-release p c1)
      (define c2 (pool-lease p (make-timeout-config #:lease 1)))
      (check-true (http-conn? c2))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pool-tests))
