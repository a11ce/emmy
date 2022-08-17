#lang emmy

(defT* Item-Type (NamedOptions Wand Flask Spell))

(%struct Item
         [type : Item-Type]
         [name : String])

(%struct Pickup
         [x : Integer]
         [y : Integer]
         [mana : Number]
         [object : Item])


(define ex
  (Pickup 12 5 3
          (Item Wand "Purple Wand")))

(browse ex)

(defλ* (incE [x : Integer]) : Integer
  "Increments an integer"
  (add1 x))

(define incE2
  ; this should warn
  (λ* ([x : Integer]) : Integer
      (add1 x)))

(defλ* (add [a : Number] [b : Number]) : Number
  "Adds two numbers"
  (+ a b))
