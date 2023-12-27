#lang racket/base

(require (prefix-in d: data/pool)
         net/http-client
         racket/contract/base
         "error.rkt"
         "logger.rkt"
         "timeout.rkt")

;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 limit/c
 pool-config?
 (contract-out
  [make-pool-config
   (->* ()
        (#:max-size limit/c
         #:idle-timeout timeout/c)
        pool-config?)]))

(struct pool-config (max-size idle-timeout)
  #:transparent)

(define limit/c
  (or/c +inf.0 exact-positive-integer?))

(define (make-pool-config
         #:max-size [max-size 128]
         #:idle-timeout [idle-timeout 600])
  (pool-config max-size idle-timeout))


;; pool ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 connector/c
 (contract-out
  [make-pool (-> pool-config? connector/c pool?)]
  [pool? (-> any/c boolean?)]
  [pool-lease (->* (pool?) ((or/c #f timeout-config?)) http-conn?)]
  [pool-release (-> pool? http-conn? void?)]
  [pool-close! (-> pool? void?)]))

(define connector/c
  (-> http-conn? http-conn?))

(struct pool (connector impl)
  #:transparent)

(define (make-pool conf connector)
  (pool
   connector
   (d:make-pool
    #:max-size (pool-config-max-size conf)
    #:idle-ttl (seconds->ms (pool-config-idle-timeout conf))
    http-conn
    http-conn-close!)))

(define (pool-lease p [t #f])
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

(define (pool-release p c)
  (d:pool-release! (pool-impl p) c))

(define (pool-close! p)
  (d:pool-close! (pool-impl p))
  (log-http-easy-debug "connection pool closed"))

(define (seconds->ms n)
  (inexact->exact (round (* n 1000))))
