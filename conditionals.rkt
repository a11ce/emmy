#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide if*)

(define-syntax (if* stx)
  (syntax-parse stx
    [(if* test:expr then:expr else:expr)
     ; needs a less-scary warning printer
     #|
     (and (equal? #t (syntax-e #'then))
          (equal? #f (syntax-e #'else))
          ((error-display-handler) "don't use if here"
           (exn:fail:syntax "???" (current-continuation-marks)
                            (list stx))))
     |#
     #'(if test then else)]
    [(if* term1:expr term2:expr term3:expr more:expr _ ...)
     (raise-syntax-error 'if "too many arguments" #'more)]
    [(if* (~optional t0:expr) (~optional t1:expr))
     (raise-syntax-error 'if "too few arguments" stx)]))
