#lang emmy

(define-type Item-Type (NamedOptions Wand Flask Spell))

(define-type Item
  (Struct [type : Item-Type]
          [name : String]))

(define-type Pickup
  (Struct [x : Integer]
          [y : Integer]
          [mana : Number]
          [object : Item]))

(define ex
  (Pickup 12 5 3
          (Item Wand "Purple Wand")))

(define (incE [x : Integer]) -> Integer
  "Increments an integer"
  (add1 x))

(define add
  (λ ([a : Number] [b : Number]) -> Number
    "Adds two numbers"
    (+ a b)))

(define (ret3) -> Number
  3)

(define map
  (λ ([p : (-> A B)] [l : (Listof A)]) -> (Listof B)
    "blah blah"
    (map p l)))