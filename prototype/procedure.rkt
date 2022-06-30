#lang racket

(require racket/struct)
(require "type.rkt")
(require "printer.rkt")
(require "debugging-support.rkt")

(provide function fn
         procedure-name procedure-arglist procedure-documentation)

(define-struct procedure ([name #:mutable] arglist implementation documentation arg-types return-type)
  #:transparent
  #:auto-value '()
  #:property prop:procedure (struct-field-index implementation)
  #:property prop:object-name (λ (p) (procedure-name p))
  #:property prop:note-name! (λ (p name)
                               (unless (procedure-name p)
                                 (set-procedure-name! p name)))
  #:methods gen:custom-write
  [(define write-proc
     (λ (obj port mode)
       (print-object-values 'Procedure
                            (list (or (procedure-name obj)
                                      "anonymous"))
                            port)))])

(define-syntax function
  (syntax-rules (->)
    [(function arg ... -> body)
     (function arg ... -> #f #f body)]
    [(function arg ... -> doc body)
     (function arg ... -> doc #f body)]
    [(function arg ... -> docstring #f body)
     (letrec ((procedure-object (procedure #f
                                           '(arg ...)
                                           (lambda (arg ...)
                                             (with-call-frame (list procedure-object arg ...)
                                               body))
                                           docstring
                                           #f
                                           #f)))
       procedure-object)]
    [(function arg ... ->
       docstring
       (arg-type ... -> return-type)
       body)
     (letrec ((procedure-object (procedure #f
                                           '(arg ...)
                                           (lambda (arg ...)
                                             (with-call-frame (list procedure-object arg ...)
                                               (begin
                                                 (check-type procedure-object arg arg-type) ...
                                                 (let ((result body))
                                                   (check-type procedure-object result return-type)
                                                   result))))
                                           docstring
                                           (list arg-type ...)
                                           return-type)))
       procedure-object)]))

(define-syntax fn (make-rename-transformer #'function))

(define-syntax-rule (check-type proc name type)
  (unless (is-a? type name)
    (raise-argument-type-error proc 'name name type)))

(define-struct (argument-type-exception exn:fail:contract) ())

(define (raise-argument-type-error proc name value type)
  (raise (argument-type-exception (format "\nInvalid type for argument ~A to procedure ~a:\n   Expected: a ~a\n   Got: ~s"
                                          name
                                          (procedure-name proc)
                                          (type-name type)
                                          value)
                                  (current-continuation-marks))
         #t))

