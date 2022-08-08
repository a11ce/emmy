#lang emmy

; this is all weird because no polymorphism yet
; + fake builtins

(defλ* (add3 [x : Number]) : Number
  "Adds 3"
  (+ 3 x))

(defλ* (cons* [a : Number] [b : (Listof Number)]) : (Listof Number)
  "Constructs a list"
  (cons a b))

(defλ* (car* [l : (Listof Number)]) : Number
  "Selects the first item of a non-empty list"
  (car l))
            
(defλ* (badNumMap [p : (-> Number Number)] [l : (Listof Number)]) : (Listof Number)
  "Constructs a new list by applying a function to each item in an existing list"
  (cons (p (car* l))
        (badNumMap p (cdr l))))

(badNumMap add3 '(1 2 3 4 5))