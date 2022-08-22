#lang typed/racket/base

(provide %λ)

(require "stack-tracing.rkt"
         "common-types.rkt"
         (for-syntax
          racket/base
          syntax/parse))

(define-syntax (%λ stx)
  (define-splicing-syntax-class proc-name
    #:description "procedure name"
    (pattern (~optional (~var n id)
                        #:defaults ([n #'#f]))))
  (define-splicing-syntax-class desc-string
    #:description "procedure description"
    (pattern (~optional (~var s string)
                        #:defaults ([s #'#f]))))

  (define-syntax-class type
    #:description "type"
    (pattern (~var t expr)))
  (define-syntax-class proc-arg
    #:description "procedure argument"
    (pattern (n:id (~datum :) t:type)))
  (define-splicing-syntax-class return-type
    #:description "return type"
    (pattern (~seq (~datum ->) t:type)))

  (syntax-parse stx
    #:datum-literals (->)
    [(_ name:proc-name (args:proc-arg ...)
        ret:return-type doc:desc-string
        body:expr ...+)
     #'(proc* 'name.n
              '(args.n ...) '(args.t ...)
              'ret.t
              doc.s
              (λ ([args.n : args.t] ...)
                (with-call-frame (call-ctx
                                  name.n
                                  (list args.n ...) '#f)
                  body ...)))]))

