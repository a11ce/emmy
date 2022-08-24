#lang racket/base

(require syntax/parse
         racket/list)

(provide type
         new-type
         maybe-tvars
         typed-decl
         return-type
         find-tvars)

(define-syntax-class type
  #:description "type"
  (pattern (~var t expr)
    #:post (~do (validate-type #'t))))

; enforces type validation
(define-syntax-class new-type
  #:description "name for type definition"
  (pattern (~var name id)
    #:fail-unless (is-cap-symbol? (syntax-e #'name))
    "new type-names must be capitalized"))

(define-splicing-syntax-class maybe-tvars
  (pattern (~optional (~seq #:forall (v:type ...)))))

(define-syntax-class typed-decl
  #:description "[name : Type]"
  (pattern (n:id (~datum :) t:type)))

(define-splicing-syntax-class return-type
  #:description "-> Return-Type"
  ; the ~or* avoids silent lack of stack traces.
  ; could be changed to error instead
  (pattern (~seq (~or* (~datum ->) (~datum :)) t:type)))

(define (validate-type ts)
  (for-each (λ (t)
              (unless (or (is-cap-symbol? t)
                          (equal? t '->))
                (eprintf "~a is probably not a valid type, try capitalizing it~n~n"
                         t)))
            (flatten (syntax->datum ts))))

(define (find-tvars ts)
  (remove-duplicates
   (filter should-be-tvar?
           (flatten (syntax->datum ts)))))

(define (is-cap-symbol? s)
  (and (symbol? s)
       (not (char-lower-case?
             (string-ref (symbol->string s) 0)))))

(define (should-be-tvar? s)
  (and (symbol? s)
       (let ([str (symbol->string s)])
         (and (= (string-length str) 1)
              (char-upper-case? (string-ref str 0))))))
