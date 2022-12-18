#lang emmy

(require typed/rackunit)

(define-type Tree (Struct))
(define-type Branch (Struct Tree
                            [left : Tree]
                            [right : Tree]))
(define-type Leaf (Struct Tree [val : Number]))

(define ex (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)))

(define (sum [tree : Tree]) -> Number
  (cond
    [(Leaf? tree) (Leaf-val tree)]
    [(Branch? tree) (+ (sum (Branch-left tree))
                       (sum (Branch-right tree)))]
    ; needed for Tree, TODO catch better
    [else (error 'bad-type)]))

(check-equal? (sum ex) 6)