#lang typed/racket/base

(require (for-syntax
          racket/base
          syntax/parse))

(provide %named-options)

(define-for-syntax (is-cap-id? stx)
  (define e (syntax-e stx))
  (and (symbol? e)
       (char-upper-case?
        (string-ref (symbol->string e) 0))))

(define-syntax (%named-options stx)
  (define-syntax-class enum-option
    #:description "named option"
    (pattern (~var opt id)
      #:fail-unless (is-cap-id? #'opt)
      "named options must be capitalized"))
  (syntax-parse stx
    [(_ name:id opts:enum-option ...+)
     #'(begin
         ; unsure if this is helpful or confusing
         (define opts 'opts) ...
         (define-type name
           (U 'opts ...)))]))
