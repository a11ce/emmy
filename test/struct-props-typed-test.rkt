#lang emmy

(require typed/rackunit)

(define-type V* (Struct
                 [a : Integer]
                 [b : Integer]
                 [c : Integer]))

(define test-v* (V* 1 2 3))
(check-equal? (V*-a test-v*) 1)

;;;

(define-type Point (Struct
                    [x : Number]
                    [y : Number]))


(define (point-+ [a : Point] [b : Point]) -> Point
  (Point (+ (Point-x a)
            (Point-x b))
         (+ (Point-y a)
            (Point-y b))))

(check-equal? (point-+ (Point 1 2)
                       (Point 3.5 4))
              (Point 4.5 6))
