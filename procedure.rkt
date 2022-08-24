#lang typed/racket/base

(provide %λ)

(require "stack-tracing.rkt"
         "common-types.rkt"
         (for-syntax
          racket/base
          syntax/parse
          "common-syntax.rkt"))

(define-syntax (%λ stx)
  (define-splicing-syntax-class proc-name
    #:description "procedure name"
    #:attributes (n)
    (pattern (~var n id))
    (pattern (~var str string)
      #:with n #'str)
    (pattern (~seq)
      #:with n #'"_"))

  (define-splicing-syntax-class desc-string
    #:description "procedure description"
    (pattern (~optional (~var s string)
                        #:defaults ([s #'#f]))))
  (define-syntax-class proc-arg
    #:description "procedure argument"
    #:attributes (n t)
    (pattern :typed-decl))

  (syntax-parse stx
    #:datum-literals (->)
    [(_ name:proc-name explicit-tvars:maybe-tvars (args:proc-arg ...)
        ret:return-type doc:desc-string
        body:expr ...+)
     (with-syntax ([opt-tvars
                    ; only look for auto tvars if not explicitly given
                    (if (attribute explicit-tvars.v)
                        #'(#:forall (explicit-tvars.v ...))
                        (with-syntax
                            ([auto-tvars (find-tvars #'(args.t ...))])
                          (if (null? (syntax-e #'auto-tvars))
                              '()
                              #'(#:forall auto-tvars))))])
       #'(proc* 'name.n
                '(args.n ...) '(args.t ...)
                'ret.t
                doc.s
                (λ (~@ . opt-tvars)
                  ([args.n : args.t] ...)
                  (with-call-frame (call-ctx
                                    name.n
                                    (list args.n ...) '#f)
                    body ...))))]))
