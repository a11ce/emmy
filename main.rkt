#lang typed/racket/base

(require (for-syntax
          racket/base))

(require "define.rkt"
         "stack-tracing.rkt"
         "define-type.rkt")

(require/typed "dagger.rkt"
               [browse (->* (Any) (Any Boolean) Any)]
               [code-mode (Parameterof Boolean)])

(provide
 browse x code-mode browse-code
 with-call-frame call-ctx browse-stack-here ; temp
 define-type
 (rename-out [define* define]
             [λ* λ])
 (except-out (all-from-out typed/racket/base)
             λ define))

(define-syntax x (make-rename-transformer #'browse))

(define-syntax-rule
  (browse-code a form)
  (parameterize ([code-mode #t])
    (parameterize ([current-namespace (namespace-anchor->namespace a)])
      (browse (quote form)))))

(define (browse-stack-here)
  (browse (current-stack) "User break" #t))

