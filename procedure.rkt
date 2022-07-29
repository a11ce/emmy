#lang typed/racket

(provide λ* defλ*)

(struct (T) proc ([name : (U False Symbol)]
                  [arguments : (Listof Symbol)]
                  [arg-types : (Listof Symbol)]
                  [return-type : Symbol]
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
                       (first (proc-arguments p))
                       (first (proc-arg-types p)))
               port)
      (for ([a (rest (proc-arguments p))]
            [t (rest (proc-arg-types p))])
        (display (format " [~a : ~a]" a t) port))
      (display ") : " port)
      (display (proc-return-type p) port))
    ;
    (case mode
      [(0) (begin
             (display (format "~a; ~a\n"
                              (or (proc-name p) "Mysterious procedure")
                              (or (proc-documentation p) "its workings are unknown"))
                      port)
             (display-typed-args))]
      [else (display (format "procedure printed with mode ~v, ???" mode) port)])))

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
     (proc 'name '(arg ...) '(type ...) 'ret-type
           string
           (λ ([arg : type] ...) body))]))

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