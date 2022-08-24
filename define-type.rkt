#lang typed/racket/base

(require (for-syntax
          racket/base
          syntax/parse)
         "struct.rkt"
         "enum.rkt")

(provide (rename-out [define-type* define-type]))


(define-syntax (define-type* stx)
  (syntax-parse stx
    #:datum-literals (NamedOptions Struct)
    [(_ name:id (NamedOptions args ...+))
     #'(%named-options name args ...)]
    [(_ name:id (Struct args ...+))
     #'(%struct name args ...)]
    [(_ else ...)
     #'(define-type else ...)]))