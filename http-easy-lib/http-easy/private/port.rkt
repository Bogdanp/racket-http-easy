#lang racket/base

(provide
 make-retaining-input-port)

(define (make-retaining-input-port p)
  (define the-box (box #f))
  (define the-port
    (make-input-port
     (object-name p)                     ; name
     p                                   ; read-in
     p                                   ; peek-in
     (lambda ()                          ; close
       (set-box! the-box #f)
       (close-input-port p))
     (lambda () (port-progress-evt p))   ; get-progress-evt
     (lambda (amt progress-evt evt)      ; commit
       (port-commit-peeked p amt progress-evt evt))
     (lambda () (port-next-location p))  ; get-location
     (lambda () (port-count-lines! p))   ; count-lines!
     p                                   ; init-position
     (case-lambda                        ; buffer-mode
       [() (file-stream-buffer-mode p)]
       [(mode) (file-stream-buffer-mode p mode)])))
  (values the-port (λ (v) (set-box! the-box v)) ))
