#lang racket

(require "type.rkt")

(provide new field simple-record record-type? record-type-fields)

(define-struct (record-type type)
  (struct-type fields constructor accessor mutator)
  #:constructor-name %make-record-type)

(define (field-position type field-name)
  (index-of (record-type-fields type)
            field-name))

(define (new RecordType . constructor-args)
  (apply (record-type-constructor RecordType)
         constructor-args))

(define (%field object field-name)
  (define t (type-of object))
  ((record-type-accessor t) object
                            (field-position t field-name)))

(define (make-record-type fields)
  (define-values (struct-type constructor predicate accessor mutator)
    (make-struct-type 'record-type
                      #f
                      (length fields)
                      0
                      #f
                      (list (cons prop:struct-type-type-object
                                  (λ () type-object))
                            (cons prop:custom-write
                                  (λ (record port mode)
                                    (define type (type-of record))
                                    (display "<" port)
                                    (display (or (type-name type)
                                                 "record")
                                             port)
                                    (for ([i (in-range (length (record-type-fields type)))]
                                          [field-name (record-type-fields type)])
                                      (display " " port)
                                      (display field-name port)
                                      (display "=" port)
                                      (write ((record-type-accessor type) record i)
                                             port))
                                    (display ">" port)
                                    )))))
  (define type-object
    (%make-record-type #f Object predicate struct-type
                       fields
                       constructor accessor mutator))
  type-object)

(define-syntax-rule (simple-record fields ...)
  (make-record-type '(fields ...)))

(define-syntax-rule (field object field-name)
  (%field object 'field-name))
