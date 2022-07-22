#lang typed/racket

(require/typed racket/struct
               [struct->list (Any -> (Listof Any))])

(provide s-name s-fields s-vals struct*)

; https://docs.racket-lang.org/ts-guide/types.html#%28part._.Types_for_.Structure_.Type_.Properties%29

; properties
(: prop:s-fields (Struct-Property (Listof Symbol)))
(: s-fields? (-> Any Boolean : (Has-Struct-Property prop:s-fields)))
(: s-fields (-> (Has-Struct-Property prop:s-fields)
                (Listof Symbol)))
(define-values (prop:s-fields s-fields? s-fields)
  (make-struct-type-property 'fields))

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

(define-syntax-rule (struct* name [fields : types] ...)
  (struct name ([fields : types] ...) #:transparent
    #:property prop:s-fields '(fields ...)
    #:property prop:s-name 'name
    #:property prop:custom-write
    (λ (s port mode)
      (display "<" port)
      (display 'name port)
      (for ([field-name '(fields ...)]
            [field-val (s-vals s)])
        (display (format " ~a=~v" field-name field-val) port))
      (display ">" port))))
