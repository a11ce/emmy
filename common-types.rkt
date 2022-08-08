#lang typed/racket/base

; this file holds common structs to avoid circular requires
; there is probably a better solution for this
(provide (struct-out call-ctx)
         (struct-out proc*)
         proc*-typestring
         Type-Symbol
         code-mode
         flat-call-stack)


(define code-mode (make-parameter #f))
(define flat-call-stack (make-parameter #t))

;;;

; covers higher-order procs
(define-type Type-Symbol (U Symbol (Sequenceof Type-Symbol)))
(struct (T) proc* ([name : (U False Symbol)]
                   [arguments : (Listof Symbol)]
                   [arg-types : (Listof Type-Symbol)]
                   [return-type : Type-Symbol]
                   [documentation : (U False String)]
                   [implementation : T])
  #:transparent
  #:property prop:procedure
  (struct-field-index implementation)
  #:property prop:custom-write
  (λ (p port mode)
    (define (display-typed-args)
      (display "(" port)
      (display (format "[~a : ~a]"
                       (car (proc*-arguments p))
                       (car (proc*-arg-types p)))
               port)
      (for ([a (cdr (proc*-arguments p))]
            [t (cdr (proc*-arg-types p))])
        (display (format " [~a : ~a]" a t) port))
      (display ") : " port)
      (display (proc*-return-type p) port))
    ;
    (case mode
      [(0 1) (display (format "{~a; ~a}"
                              (or (proc*-name p) "Mysterious procedure")
                              (or (proc*-documentation p) "its workings are unknown"))
                      port)]
      [(#f) (display (or (proc*-name p) "mysterious procedure") port)]
      [else (display (format "procedure printed with mode ~v, ???" mode) port)]))
  )

(define (proc*-typestring p);[p : (All (T) (proc* T))])
  ; avoids issues with polymorphism when used from untyped racket
  ; only used internally so this should be fine
  (if (proc*? p)
      (apply string-append
             (list (format "([~a : ~a]"
                           (car (proc*-arguments p))
                           (car (proc*-arg-types p)))
                   (apply string-append
                          (for/list ([a : Type-Symbol (cdr (proc*-arguments p))]
                                     [t : Type-Symbol (cdr (proc*-arg-types p))])
                            : (Listof String)
                            (format " [~a : ~a]" a t)))
                   (format ") : ~a" (proc*-return-type p))))
      "meow"))

;;;

(struct call-ctx
  ([proc : Any]
   [args : (Listof Any)]
   [outer-call : (U call-ctx False)])
  #:transparent
  #:mutable)
