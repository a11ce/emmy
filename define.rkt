#lang typed/racket/base

(require (for-syntax
          racket/base
          racket/syntax
          syntax/parse
          "common-syntax.rkt")
         "procedure.rkt")

(provide (rename-out [%λ λ*])
         define*)


(define-syntax (define* stx)
  
  (define-syntax-class proc-arg
    #:description "procedure argument"
    #:attributes (n t)
    (pattern :typed-decl))
  
  (syntax-parse stx
    #:literals ((λ %λ))
    [(_ explicit-tvars:maybe-tvars (proc-name:id args:proc-arg ...)
        ret:return-type body ...)
     (with-syntax* ([tvars (if (attribute explicit-tvars.v)
                               #'(explicit-tvars.v ...)
                               (find-tvars #'(args.t ...)))]
                    [t-ann (if (null? (syntax-e #'tvars))
                               #'(: proc-name : (-> args.t ... ret.t))
                               #'(: proc-name : (All tvars (-> args.t ... ret.t))))]
                    [fall-tvars (if (null? (syntax-e #'tvars))
                                    '()
                                    #'(#:forall tvars))])
       
       #'(begin
           t-ann
           (define proc-name
             (%λ proc-name (~@ . fall-tvars)
                 (args ...) -> ret.t
                 body ...))))]
    [(_ proc-name:id (λ (args ...) body ...))
     #'(define* (proc-name args ...) body ...)]
    [(_ name:id val:expr)
     #'(define name val)]))
