#lang typed/racket/base

(require "struct.rkt"
         "procedure.rkt"
         "stack.rkt")

(require/typed "dagger.rkt"
               [browse (->* (Any) (Any) Void)]
               [code-mode (Parameterof Boolean)])

(provide
 browse code-mode browse-code
 with-call-frame call-ctx ; temp
 (all-from-out typed/racket/base
               "struct.rkt"
               "procedure.rkt"))

(define-syntax-rule
  (browse-code a form)
  (parameterize ([code-mode #t])
    (parameterize ([current-namespace (namespace-anchor->namespace a)])
      (browse (quote form)))))

; XXX this is here only to avoid circular imports
(let ([orig-handler (error-display-handler)])
  (error-display-handler
   (λ ([msg : String] [exn : Any])
     (if (exn? exn)
         (let ([stack (exn-stack exn)])
           (build-stacked-ctx! stack)
           (browse (car stack) msg))
         (displayln "meow"))
     (orig-handler msg exn))))