#lang typed/racket/base

(require "common-types.rkt"
         (for-syntax
          racket/base
          syntax/parse
          "common-syntax.rkt"))

(require/typed racket/struct
               [struct->list (Any -> (Listof Any))])

(provide s-name s-fields s-types s-vals %struct s-name?)

; https://docs.racket-lang.org/ts-guide/types.html#%28part._.Types_for_.Structure_.Type_.Properties%29

; properties
(: prop:s-fields (Struct-Property (Listof Symbol)))
(: s-fields? (-> Any Boolean : (Has-Struct-Property prop:s-fields)))
(: s-fields (-> (Has-Struct-Property prop:s-fields)
                (Listof Symbol)))
(define-values (prop:s-fields s-fields? s-fields)
  (make-struct-type-property 'fields))

(: prop:s-types (Struct-Property (Listof Type-Symbol)))
(: s-types? (-> Any Boolean : (Has-Struct-Property prop:s-types)))
(: s-types (-> (Has-Struct-Property prop:s-types)
               (Listof Type-Symbol)))
(define-values (prop:s-types s-types? s-types)
  (make-struct-type-property 'types))

(: prop:s-name (Struct-Property Symbol))
(: s-name? (-> Any Boolean : (Has-Struct-Property prop:s-name)))
(: s-name (-> (Has-Struct-Property prop:s-name)
              Symbol))
(define-values (prop:s-name s-name? s-name)
  (make-struct-type-property 'name))

; not a property but should mostly act like it
(define (s-vals s)
  (struct->list s))

;;;

(define-syntax (%struct stx)
  (define-syntax-class struct-name
    #:description "struct name"
    #:attributes (name)
    (pattern :new-type))
  (define-syntax-class field-def
    #:description "struct field definition"
    #:attributes (n t)
    (pattern :typed-decl))
  (define-splicing-syntax-class kwargs
    #:description "optional struct keywords"
    (pattern (~optional (~var kwarg keyword))))

  (syntax-parse stx
    #:datum-literals (:)
    [(_ name:struct-name opt-args:kwargs fields:field-def ...+)
     #'(struct name ([fields.n : fields.t] ...)
         (~? (~@ . opt-args))
         #:transparent
         #:property prop:s-fields '(fields.n ...)
         #:property prop:s-types  '(fields.t ...)
         #:property prop:s-name 'name
         #:property prop:custom-write
         (λ (s port mode)
           (display "<" port)
           (display 'name port)
           (for ([field-name '(fields.n ...)]
                 [field-val (s-vals s)])
             (display (format " ~a=~v" field-name field-val) port))
           (display ">" port)))]))
