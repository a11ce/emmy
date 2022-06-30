#lang racket

(require racket/struct)

(provide type type? type-of type-name is-a? Object Integer String Procedure
         prop:struct-type-type-object
         prop:note-name! note-name!)

(define-values (prop:struct-type-type-object struct-type-type-object? struct-type-type-object)
  (make-struct-type-property 'struct-type-type-object))

(define-values (prop:note-name! has-name-noter? name-noter)
  (make-struct-type-property 'note-name!))

(define (note-name! object name)
  (when (has-name-noter? object)
    ((name-noter object) object name)))

(define-struct type ([name #:mutable] supertype predicate [subtypes #:mutable #:auto])
  #:transparent
  #:auto-value '()
  #:property prop:note-name! (λ (t name)
                               (unless (type-name t)
                                 (set-type-name! t name)))
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'Type)
      (lambda (obj) (list (type-name obj)))))])

(define (is-a? type object)
  ((type-predicate type) object))

(define (type-of object)
  (cond [(struct-type-type-object? object)
         ((struct-type-type-object object))]
        [else
         (define (search type)
           (define subtype (findf (λ (t) (is-a? t object))
                                  (type-subtypes type)))
           (if subtype
               (search subtype)
               type))
         (search Object)]))

(define Object (type 'Object #f (λ (x) #t)))

(define (add-subtype! sub)
  (define super (type-supertype sub))
  (set-type-subtypes! super
                      (cons sub (type-subtypes super))))

(define (primitive-type name predicate)
  (define t (type name Object predicate))
  (add-subtype! t)
  t)

(define Integer (primitive-type 'Integer integer?))
(define String (primitive-type 'String string?))
(define Procedure (primitive-type 'Procedure procedure?))