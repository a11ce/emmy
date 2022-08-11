#lang typed/racket/base

(require "struct.rkt"
         "procedure.rkt"
         "stack-tracing.rkt")

(require/typed "dagger.rkt"
               [browse (->* (Any) (Any Boolean) Any)]
               [code-mode (Parameterof Boolean)])

(provide
 browse x code-mode browse-code
 with-call-frame call-ctx browse-stack-here ; temp
 (all-from-out typed/racket/base
               "struct.rkt"
               "procedure.rkt"))

(define-syntax x (make-rename-transformer #'browse))

(define-syntax-rule
  (browse-code a form)
  (parameterize ([code-mode #t])
    (parameterize ([current-namespace (namespace-anchor->namespace a)])
      (browse (quote form)))))

(define (browse-stack-here)
  (browse (current-stack) "User break" #t))

