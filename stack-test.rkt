#lang emmy

; this is all weird because no polymorphism yet
; + fake builtins

(defλ* (add3 [x : Number]) : Number
  "Adds 3"
  (with-call-frame (call-ctx add3 (list x) #f)
    (+ 3 x)))

(defλ* (cons* [a : Number] [b : (Listof Number)]) : (Listof Number)
  "Constructs a list"
  (with-call-frame (call-ctx cons* (list a b) #f)
    (cons a b)))

(defλ* (car* [l : (Listof Number)]) : Number
  "Selects the first item of a non-empty list"
  (with-call-frame (call-ctx car* (list l) #f)
    (car l)))
            
(defλ* (badNumMap [p : (-> Number Number)] [l : (Listof Number)]) : (Listof Number)
  "Constructs a new list by applying a function to each item in an existing list"
  (with-call-frame (call-ctx badNumMap (list p l) #f)
    (cons (p (car* l))
          (badNumMap p (cdr l)))))

(badNumMap add3 '(1 2 3 4 5))