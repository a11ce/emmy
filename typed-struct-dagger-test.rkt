#lang typed/racket

(require "struct-props-typed.rkt")
(require/typed "dagger.rkt"
               [browse (Any -> Void)])

(define-type Item-Type (U 'wand 'flask 'spell))

(struct* Item
         [type : Item-Type]
         [name : String])

(struct* Pickup
         [x : Integer]
         [y : Integer]
         [object : Item])


(define ex
  (Pickup 12 5
          (Item 'wand "Purple Wand")))

(browse ex)