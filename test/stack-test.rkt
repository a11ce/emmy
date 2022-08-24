#lang emmy

(require typed/rackunit)

(define (add3 [x : Number]) -> Number
  "Adds 3"
  (+ 3 x))

(define (cons* [a : A] [b : (Listof B)]) -> (Listof (U A B))
  "Constructs a list"
  (cons a b))

(define (car* [l : (Listof A)]) -> A
  "Selects the first item of a non-empty list"
  (car l))

(define (map [p : (-> A B)] [l : (Listof A)]) -> (Listof B)
  (if (null? l)
      '()
      (cons* (p (car* l))
             (map p (cdr l)))))

(check-equal? (map add3 '(1 2 3 4 5))
              '(4 5 6 7 8))

(define (badMap [p : (-> A B)] [l : (Listof A)]) -> (Listof B)
  (cons* (p (car* l))
         (badMap p (cdr l))))

(check-exn #rx"car: contract"
           (λ () -> Any (badMap add3 '(1 2 3 4 5))))