#lang racket/base

(require (prefix-in d: data/pool)
         net/http-client
         racket/contract
         "error.rkt"
         "logger.rkt"
         "timeout.rkt")

;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 limit/c
 make-pool-config
 pool-config?)

(struct pool-config (max-size idle-timeout)
  #:transparent)

(define limit/c
  (or/c +inf.0 exact-positive-integer?))

(define/contract (make-pool-config
                  #:max-size [max-size 128]
                  #:idle-timeout [idle-timeout 600])
  (->* ()
       (#:max-size limit/c
        #:idle-timeout timeout/c)
       pool-config?)
  (pool-config max-size idle-timeout))


;; pool ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 connector/c
 make-pool
 pool?
 pool-lease
 pool-release
 pool-close!)

(define connector/c
  (-> http-conn? http-conn?))

(struct pool (connector impl)
  #:transparent)

(define/contract (make-pool conf connector)
  (-> pool-config? connector/c pool?)
  (pool
   connector
   (d:make-pool
    #:max-size (pool-config-max-size conf)
    #:idle-ttl (seconds->ms (pool-config-idle-timeout conf))
    http-conn
    http-conn-close!)))

(define/contract (pool-lease p [t #f])
  (->* (pool?) ((or/c #f timeout-config?)) http-conn?)
  (define impl (pool-impl p))
  (define maybe-lease-timeout-ms
    (and t (seconds->ms (timeout-config-lease t))))
  (cond
    [(d:pool-take! impl maybe-lease-timeout-ms)
     => (lambda (leased-c)
          (define out (make-channel))
          (define thd
            (thread
             (lambda ()
               (with-handlers ([exn:fail?
                                (lambda (e)
                                  (channel-put out e))])
                 (channel-put out ((pool-connector p) leased-c))))))
          (define res
            (sync/timeout (and t (timeout-config-connect t)) out))

          (cond
            [(exn:fail? res)
             (log-http-easy-warning "connection failed: ~a" (exn-message res))
             (http-conn-close! leased-c)
             (d:pool-release! impl leased-c)
             (raise res)]

            [(not res)
             (log-http-easy-warning "connection timed out")
             (kill-thread thd)
             (http-conn-close! leased-c)
             (d:pool-release! impl leased-c)
             (raise (make-timeout-error 'connect))]

            [else
             res]))]

    [else
     (raise (make-timeout-error 'lease))]))

(define/contract (pool-release p c)
  (-> pool? http-conn? void?)
  (d:pool-release! (pool-impl p) c))

(define/contract (pool-close! p)
  (-> pool? void?)
  (d:pool-close! (pool-impl p))
  (log-http-easy-debug "connection pool closed"))

(define (seconds->ms n)
  (inexact->exact (round (* n 1000))))
