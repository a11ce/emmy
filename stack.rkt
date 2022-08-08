#lang typed/racket/base

(require racket/function
         racket/match
         "stack-struct.rkt")

(provide (struct-out call-ctx)
         with-call-frame exn-stack build-stacked-ctx!)

(define call-frame-key (make-continuation-mark-key 'call))

(define (current-stack)
  (continuation-mark-set->list (current-continuation-marks)
                               call-frame-key))
(define (exn-stack [exn : exn])
  (continuation-mark-set->list (exn-continuation-marks exn)
                               call-frame-key))

; dagger treats lists as 'flat',
; which almost always makes more sense.
; this is an exception
(: build-stacked-ctx! : (Listof Any) -> Void)
(define (build-stacked-ctx! ctxs)
  (match ctxs
    [(list last) (void)]
    [(list* (? call-ctx? cur)
            (? call-ctx? next)
            rest)
     (set-call-ctx-outer-call! cur next)
     (build-stacked-ctx! (cdr ctxs))]))

(define-syntax-rule (with-call-frame data form)
  (with-continuation-mark call-frame-key data
    (identity ; needed to create a new frame
     form)))