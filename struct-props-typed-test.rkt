#lang emmy

(require typed/rackunit)

(struct* V*
         [a : Integer]
         [b : Integer]
         [c : Integer])
(define test-v* (V* 1 2 3))
(check-equal? (s-fields test-v*)
              '(a b c))
(check-equal? (s-types test-v*)
              '(Integer Integer Integer))
(check-equal? (s-name test-v*)
              'V*)
;;;

(struct* Point
         [x : Number]
         [y : Number])

(: point-+ (-> Point Point Point))
(define (point-+ a b)
  (Point (+ (Point-x a)
            (Point-x b))
         (+ (Point-y a)
            (Point-y b))))

(check-equal? (point-+ (Point 1 2)
                       (Point 3.5 4))
              (Point 4.5 6))
