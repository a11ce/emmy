#lang emmy

(define-namespace-anchor ns-a)

(defλ* (+* [a : Number] [b : Number]) : Number
  "adds two numbers"
  (+ a b))

(browse-code ns-a (+* 1 (+* 3 5)))