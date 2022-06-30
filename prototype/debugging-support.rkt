#lang racket

(provide call-frame-key current-call-stack with-call-frame exn-call-stack)

(define call-frame-key (make-continuation-mark-key 'call))

(define (current-call-stack)
  (continuation-mark-set->list (current-continuation-marks)
                               call-frame-key))

(define (exn-call-stack exn)
  (continuation-mark-set->list (exn-continuation-marks exn)
                               call-frame-key))

(define-syntax-rule (with-call-frame data form)
  (with-continuation-mark call-frame-key data
    form))