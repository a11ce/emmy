#lang emmy

; this is all weird because no polymorphism yet
; + fake builtins

(define (add3 [x : Number]) -> Number
  "Adds 3"
  (+ 3 x))

(define (cons* [a : Number] [b : (Listof Number)]) -> (Listof Number)
  "Constructs a list"
  (cons a b))

(define (car* [l : (Listof Number)]) -> Number
  "Selects the first item of a non-empty list"
  (car l))

(define (badNumMap [p : (-> Number Number)] [l : (Listof Number)]) -> (Listof Number)
  "Constructs a new list by applying a function to each item in an existing list"
  (cons* (p (car* l))
        (badNumMap p (cdr l))))

;(badNumMap add3 '(1 2 3 4 5))

(define (numMap [p : (-> Number Number)] [l : (Listof Number)]) -> (Listof Number)
  "Constructs a new list by applying a function to each item in an existing list"
  (browse-stack-here)
  (if (null? l)
      '()
      (cons* (p (car* l))
             (numMap p (cdr l)))))

(numMap add3 '(1 2 3 4 5))
