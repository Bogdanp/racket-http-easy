#lang racket/base

(require (for-syntax racket/base
                     racket/path
                     setup/getinfo))

(provide
 lib-version)

(begin-for-syntax
  (define this-path (simplify-path (build-path (path-only (syntax-source #'here)) 'up 'up)))
  (define info-ref (get-info/full this-path)))

(define-syntax (lib-version stx)
  (syntax-case stx ()
    [(_) (datum->syntax stx (info-ref 'version))]))
