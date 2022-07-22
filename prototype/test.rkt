#lang racket

(require "emmy.rkt")

(define f
  (function x ->
    "add 1" (Integer -> Integer)
    (+ x 1)))

(define (increment x) ->
  "Adds one to its argument" (Integer -> Integer)
  (+ x 1))

(define (my-map p l) ->
  "Returns the value of p for each element of l"
  (if (empty? l)
      '()
      (cons (p (first l))
            (my-map p (rest l)))))

(define (test) ->
  (my-map (fn x -> (+ x 1))
          '(1 2 "three" 4 5)))



(define V (simple-record x y z))

(field (new V 1 2 3) x)

(define (plus a b c) -> (+ a b c))
(define (times a b) -> (* a b))

(define (bad-math) ->
  (plus 1
     (- 3
        (times 4 "five"))
     6))

;(debug)
