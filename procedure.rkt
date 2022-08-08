#lang typed/racket/base

(provide λ* defλ* proc*? proc*-typestring)

(require "stack-tracing.rkt"
         "common-types.rkt")

(define-syntax λ*
  (syntax-rules (:)
    ; no name or string
    [(λ* ([arg : type] ...) : ret-type
         body)
     (λ* (#f [arg : type] ...) : ret-type
         #f
         body)]
    ; name/ no string
    [(λ* (name [arg : type] ...) : ret-type
         body)
     (λ* (name [arg : type] ...) : ret-type
         #f body)]
    ; yay good
    [(λ* (name [arg : type] ...) : ret-type
         string body)
     (proc* 'name '(arg ...) '(type ...) 'ret-type
            string
            (λ ([arg : type] ...)
              (with-call-frame (call-ctx name (list arg ...) #f)
                body)))]))

(define-syntax defλ*
  (syntax-rules (:)
    [(defλ* (name [arg : type] ...) : ret-type
       ; body actually means body-and-maybe-doc
       body ...)
     (begin
       (: name : (-> type ... ret-type))
       (define name
         (λ* (name [arg : type] ...) : ret-type
             body ...)))]))

