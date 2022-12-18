#lang typed/racket/base/no-check

(require emmy/conditionals
         typed/rackunit
         syntax/macro-testing)

(check-exn exn:fail:syntax?
           (λ () (convert-compile-time-error (if* 1 2)))
           "too few")

(check-exn exn:fail:syntax?
           (λ () (convert-compile-time-error (if* 1 2 3 4)))
           "too many")

(check-exn exn:fail:syntax?
           (λ () (convert-compile-time-error (if* 1 #:two 3)))
           "too many")

(check-exn exn:fail:syntax?
           (λ () (convert-compile-time-error (if* #:cond 1 2)))
           "expected expression")

(module val-tests emmy
  (require typed/rackunit)
  (check-equal? (if #t 1 2) 1)
  (check-equal? (if 0 1 2) 1)
  (check-equal? (if 'false 1 2) 1)

  ; empty = false might be nice
  (check-equal? (if '() 1 2) 1)

  (check-equal? (if #f 1 2) 2))

