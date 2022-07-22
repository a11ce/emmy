#lang typed/racket

(require typed/rackunit)

(define-syntax-rule
  (short-type form)
  (parameterize ([current-output-port (open-output-string)])
    (#%top-interaction . form)
    (let ([matched (regexp-match
             #rx"- : (.*?)\\ (?:\\.\\.\\.\\ )?\\["
             (get-output-string (current-output-port)))])
      (and (list? matched)
          (second matched)))))

(check-equal? (short-type +)   "(-> Number * Number)")
(check-equal? (short-type 3)   "Integer")
(check-equal? (short-type 1.2) "Flonum")
