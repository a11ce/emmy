#lang info

(define collection "emmy")

(define scribblings '(("scribblings/emmy.scrbl")))

(define compile-omit-paths '("etc"))
(define test-omit-paths '("etc"))

(define deps '("base"
               "gui-lib"
               "htdp-lib"
               "rackunit-lib"
               "rackunit-typed"
               "srfi-lite-lib"
               "typed-racket-lib"))

(define build-deps '("scribble-lib"))
