#lang racket/base

(require net/http-client
         racket/contract
         racket/match
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
  (pool-config max-size
               idle-timeout))


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

(struct pool (custodian connector mgr)
  #:transparent)

(define/contract (make-pool config connector)
  (-> pool-config? connector/c pool?)
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (pool custodian connector (make-pool-manager config))))

(define (make-pool-manager conf)
  (thread
   (lambda ()
     (match-define (pool-config max-size idle-timeout)
       conf)

     (define released-evt (make-semaphore))
     (define (make-deadline)
       (+ (current-inexact-milliseconds)
          (* idle-timeout 1000)))

     (let loop ([conns  null]
                [active null]
                [idle   null]
                [deads  (hasheq)]
                [waits  null])
       (define idle-timeout-evt
         (if (hash-empty? deads)
             never-evt
             (alarm-evt (apply min (hash-values deads)))))

       (sync
        (handle-evt
         (thread-receive-evt)
         (lambda (_)
           (match (thread-receive)
             [(list 'close)
              (for-each http-conn-close! conns)
              (log-http-easy-debug "pool manager stopped")]

             [(list 'lease ch)
              (cond
                [(not (null? idle))
                 (log-http-easy-debug "leasing existing connection")
                 (define c (car idle))
                 (channel-put ch c)
                 (loop conns (cons c active) (cdr idle) (hash-remove deads c) waits)]

                [(< (length conns) max-size)
                 (log-http-easy-debug "leasing new connection")
                 (define c (http-conn))
                 (channel-put ch c)
                 (loop (cons c conns) (cons c active) idle deads waits)]

                [else
                 (log-http-easy-debug "no connections available at this time")
                 (loop conns active idle deads (cons ch waits))])]

             [(list 'release c)
              (define new-active (remq c active))
              (cond
                [(equal? active new-active)
                 (log-http-easy-warning "connection released multiple times")
                 (loop conns active idle deads waits)]

                [else
                 (log-http-easy-debug "connection released")
                 (semaphore-post released-evt)
                 (loop conns new-active (cons c idle) (hash-set deads c (make-deadline)) waits)])])))

        (handle-evt
         released-evt
         (lambda (_)
           (cond
             [(null? waits)
              (log-http-easy-debug "no waiters to lease connections to")
              (loop conns active idle deads waits)]

             [(null? idle)
              (log-http-easy-debug "no connections to lease to waiters")
              (loop conns active idle deads waits)]

             [else
              (log-http-easy-debug "leasing connection to waiter")
              (define ch (car waits))
              (define c (car idle))
              (channel-put ch c)
              (loop conns (cons c active) (cdr idle) (hash-remove deads c) (cdr waits))])))

        (handle-evt
         idle-timeout-evt
         (lambda (_)
           (log-http-easy-debug "checking for expired idle connections")
           (define now (current-inexact-milliseconds))
           (define-values (dead live)
             (for/fold ([dead (hasheq)]
                        [live (hasheq)])
                       ([(c d) (in-hash deads)])
               (if (< d now)
                   (values (hash-set dead c d) live)
                   (values dead (hash-set live c d)))))

           (cond
             [(hash-empty? dead)
              (log-http-easy-debug "no idle connections to expire")
              (loop conns active idle deads waits)]

             [else
              (log-http-easy-debug "expiring ~a idle connections" (hash-count dead))
              (define new-conns (filter (lambda (c) (not (hash-has-key? dead c))) conns))
              (define new-idle (filter (lambda (c) (not (hash-has-key? dead c))) conns))
              (loop new-conns active new-idle live waits)]))))))))

(define-syntax-rule (send p msg arg ...)
  (thread-send (pool-mgr p) (list 'msg arg ...)))

;; TODO: Handle breaks.
(define/contract (pool-lease p [t #f])
  (->* (pool?) ((or/c false/c timeout-config?)) http-conn?)
  (define ch (make-channel))
  (send p lease ch)
  (cond
    [(sync/timeout (and t (timeout-config-lease t)) ch)
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
             (pool-release p leased-c)
             (raise res)]

            [(not res)
             (log-http-easy-warning "connection timed out")
             (kill-thread thd)
             (http-conn-close! leased-c)
             (pool-release p leased-c)
             (raise (make-timeout-error 'connect))]

            [else
             res]))]

    [else
     (async-release p ch)
     (raise (make-timeout-error 'lease))]))

(define (async-release p ch)
  (thread
   (lambda ()
     (sync
      (handle-evt
       ch
       (lambda (c)
         (log-http-easy-warning "releasing orphan connection")
         (pool-release p c)))))))

(define/contract (pool-release p c)
  (-> pool? http-conn? void?)
  (send p release c))

(define/contract (pool-close! p)
  (-> pool? void?)
  (send p close)
  (thread-wait (pool-mgr p))
  (custodian-shutdown-all (pool-custodian p))
  (log-http-easy-debug "connection pool closed"))
