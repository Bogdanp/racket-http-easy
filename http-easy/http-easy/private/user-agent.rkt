#lang racket/base

(require racket/contract
         racket/port
         "reflect.rkt")

(provide
 current-user-agent)

(define/contract current-user-agent
  (parameter/c (or/c bytes? string?))
  (make-parameter (call-with-output-bytes
                   (lambda (out)
                     (fprintf out "net/http-easy (~a; racket[~a] ~a; ~a)"
                              (system-type 'os)
                              (case (system-type 'vm)
                                [(chez-scheme) 'CS]
                                [else 'BC])
                              (version)
                              (lib-version))))))
