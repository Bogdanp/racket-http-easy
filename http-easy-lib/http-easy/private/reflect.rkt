#lang racket/base

(require (for-syntax racket/base
                     racket/path
                     setup/getinfo))

(provide
 lib-version)

(begin-for-syntax
  (define this-path (build-path (path-only (syntax-source #'here)) 'up 'up))
  (define info-ref (get-info/full this-path)))

(define-syntax (get-lib-version stx)
  (datum->syntax stx (info-ref 'version)))

(define (lib-version)
  (get-lib-version))
