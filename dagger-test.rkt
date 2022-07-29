#lang typed/racket

(require "struct.rkt"
         "procedure.rkt")
(require/typed "dagger.rkt"
               [browse (Any -> Void)])

(define-type Item-Type (U 'wand 'flask 'spell))

(struct* Item
         [type : Item-Type]
         [name : String])

(struct* Pickup
         [x : Integer]
         [y : Integer]
         [mana : Number]
         [object : Item])


(define ex
  (Pickup 12 5 3
          (Item 'wand "Purple Wand")))

;(browse ex)

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
