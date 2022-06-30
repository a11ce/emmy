#lang racket

(require racket/gui)
(require [only-in srfi/1 iota])
(require racket/struct-info)
(require racket/struct)

(provide browse)

(define-struct test (a b) #:transparent)

(define test-instance (test 1 2))

(define (shallow-print->string object)
  (~s object #:max-width 200))

(define (browse object)
  (define frame (new frame%
                     [label "Browser"]))
  (define panel (new vertical-panel%
                     [parent frame]))
  (define slots '())
  (define history '())
  (define current-object #f)
  (define back (new button%
                    [parent panel]
                    [label "Back"]
                    [callback (λ (b e)
                                (pop-object!))]))
  (define listbox (new list-box%
                       [parent panel]
                       [label #f]
                       [columns '("Field" "Value")]
                       [choices '()]
                       [min-width 500]
                       [min-height 300]
                       [callback (λ (b e)
                                   (define selection
                                     (first (send listbox get-selections)))
                                   (push-object! (second (list-ref slots
                                                                   selection))))]))
  (define (push-object! o)
    (set! history (cons current-object history))
    (set-object! o))
  (define (pop-object!)
    (unless (empty? history)
      (set-object! (first history))
      (set! history (rest history))))
  (define (set-object! o)
    (define-values (header slot-names slot-values)
      (explode o))
    (set! current-object object)
    (set! slots slot-values)
    (send frame set-status-text header)
    (send listbox set
          slot-names
          (map first slot-values)))
  (send listbox set-column-width 1 400 400 400)
  (set-object! object)
  (send frame show #t))

(define (explode object)
  (define-values (type is-struct?)
    (struct-info object))
  (cond [(list? object)
         (values "List"
                 (map ~a (iota (length object)))
                 (map (λ (elt)
                        (list (shallow-print->string elt)
                              elt))
                      object))]      
        [else
         (values "Object"
                 (list "Value")
                 (list (list (shallow-print->string object)
                             #f)))]))

