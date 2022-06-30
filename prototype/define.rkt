#lang racket

(require (only-in "type.rkt"
                  note-name!))
(require "procedure.rkt")
(provide (rename-out [new-define define]))

(define-syntax new-define
  (syntax-rules (->)
    [(new-define variable value)
     (define variable
       (let ((v value))
         (note-name! v 'variable)
         v))]
    [(new-define (variable args ...) -> elements ...)
     (define variable
       (let ((v (function args ... -> elements ...)))
         (note-name! v 'variable)
         v))]))
