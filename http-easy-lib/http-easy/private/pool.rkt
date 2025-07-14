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
   (->* []
        [#:max-size limit/c
         #:idle-timeout timeout/c]
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
  [pool-lease (->* [pool?] [(or/c #f timeout-config?)] http-conn?)]
  [pool-release (-> pool? http-conn? void?)]
  [pool-abandon (-> pool? http-conn? void?)]
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
  (define impl
    (pool-impl p))
  (define timeout
    (and t (seconds->ms (timeout-config-lease t))))
  (define leased-conn
    (d:pool-take! impl timeout))
  (unless leased-conn
    (raise (make-timeout-error 'lease)))
  (define out (make-channel))
  (define thd
    (thread
     (lambda ()
       (with-handlers ([exn:break? void]
                       [exn:fail? (λ (e) (channel-put out e))])
         (channel-put out ((pool-connector p) leased-conn))))))
  (define conn-or-exn
    (sync/timeout (and t (timeout-config-connect t)) out))
  (cond
    [(exn:fail? conn-or-exn)
     (log-http-easy-warning "connection failed: ~a" (exn-message conn-or-exn))
     (d:pool-abandon! impl leased-conn)
     (raise conn-or-exn)]
    [(not conn-or-exn)
     (log-http-easy-warning "connection timed out")
     (d:pool-abandon! impl leased-conn)
     (break-thread thd)
     (raise (make-timeout-error 'connect))]
    [else conn-or-exn]))

(define (pool-release p c)
  (d:pool-release! (pool-impl p) c))

(define (pool-abandon p c)
  (d:pool-abandon! (pool-impl p) c))

(define (pool-close! p)
  (d:pool-close! (pool-impl p))
  (log-http-easy-debug "connection pool closed"))

(define (seconds->ms n)
  (inexact->exact (round (* n 1000))))
