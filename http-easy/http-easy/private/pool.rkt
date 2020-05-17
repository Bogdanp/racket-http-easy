#lang racket/base

(require net/http-client
         racket/contract
         racket/match
         "error.rkt"
         "logger.rkt"
         "timeout.rkt")

(provide
 make-pool-config
 pool-config?

 connector/c
 make-pool
 pool?
 pool-lease
 pool-release
 pool-shutdown!)

;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct pool-config (max-size)
  #:transparent)

(define/contract (make-pool-config #:max-size [max-size 128])
  (->* ()
       (#:max-size (or/c +inf.0 exact-positive-integer?))
       pool-config?)
  (pool-config max-size))


;; pool ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define connector/c
  (-> http-conn? http-conn?))

(struct pool (custodian connector mgr)
  #:transparent)

(define/contract (make-pool config connector)
  (-> pool-config? connector/c pool?)
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (pool custodian connector (make-pool-manager config))))

;; TODO: Idle timeouts.
(define (make-pool-manager conf)
  (thread
   (lambda ()
     (define max-size (pool-config-max-size conf))
     (define released-evt (make-semaphore))

     (let loop ([conns  null]
                [active null]
                [idle   null]
                [waits  null])
       (sync
        (handle-evt
         (thread-receive-evt)
         (lambda (_)
           (match (thread-receive)
             [(list 'shutdown)
              (for-each http-conn-close! conns)
              (log-http-easy-debug "pool manager shut down")]

             [(list 'lease ch)
              (cond
                [(not (null? idle))
                 (log-http-easy-debug "leasing existing connection")
                 (define c (car idle))
                 (channel-put ch c)
                 (loop conns (cons c active) (cdr idle) waits)]

                [(< (length conns) max-size)
                 (log-http-easy-debug "leasing new connection")
                 (define c (http-conn))
                 (channel-put ch c)
                 (loop (cons c conns) (cons c active) idle waits)]

                [else
                 (log-http-easy-debug "no connections available at this time")
                 (loop conns active idle (cons ch waits))])]

             [(list 'release c)
              (define new-active (remq c active))
              (cond
                [(equal? active new-active)
                 (log-http-easy-warning "connection released multiple times")
                 (loop conns active idle waits)]

                [else
                 (log-http-easy-debug "connection released")
                 (semaphore-post released-evt)
                 (loop conns new-active (cons c idle) waits)])])))

        (handle-evt
         released-evt
         (lambda (_)
           (cond
             [(or (null? idle) (null? waits))
              (loop conns active idle waits)]

             [else
              (log-http-easy-debug "leasing connection to waiter")
              (define ch (car waits))
              (define c (car idle))
              (channel-put ch c)
              (loop conns (cons c active) (cdr idle) (cdr waits))]))))))))

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
             (pool-release p leased-c)
             (raise res)]

            [(not res)
             (log-http-easy-warning "connection timed out")
             (kill-thread thd)
             (pool-release p leased-c)
             (raise (make-timeout-error 'connect))]

            [else
             res]))]

    [else
     (thread
      (lambda ()
        (sync
         (handle-evt
          ch
          (lambda (c)
            (log-http-easy-warning "releasing orphan connection")
            (pool-release p c))))))
     (raise (make-timeout-error 'lease))]))

(define/contract (pool-release p c)
  (-> pool? http-conn? void?)
  (send p release c))

(define/contract (pool-shutdown! p)
  (-> pool? void?)
  (define mgr (pool-mgr p))
  (thread-send mgr (list 'shutdown))
  (thread-wait mgr)
  (custodian-shutdown-all (pool-custodian p))
  (log-http-easy-debug "connection pool shut down"))
