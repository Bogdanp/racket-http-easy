#lang racket/base

(require racket/contract/base
         racket/port
         "reflect.rkt")

(provide
 (contract-out
  [current-user-agent (parameter/c (or/c bytes? string?))]))

(define current-user-agent
  (make-parameter (call-with-output-bytes
                   (lambda (out)
                     (fprintf out "net/http-easy (~a; racket[~a] ~a; ~a)"
                              (system-type 'os)
                              (case (system-type 'vm)
                                [(chez-scheme) 'CS]
                                [else 'BC])
                              (version)
                              (lib-version))))))
