#lang racket

(provide print-object-values)

(define (print-object-values type-name values port)
  (display "<" port)
  (display type-name port)
  (for-each (λ (v)
              (display " " port)
              (write v port))
            values)
  (display ">" port))

