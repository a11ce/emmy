#lang racket

(require "define.rkt"
         "procedure.rkt"
         "type.rkt"
         "debugging-support.rkt"
         "object-browser.rkt"
         "record-type.rkt"
         (prefix-in dagger: "../dagger.rkt")
         racket/exn)

(provide define
         function fn procedure-arglist procedure-documentation
         (all-from-out "type.rkt")
         #%top-interaction browse debug
         debug/dagger dagger:browse
         new simple-record field)

(define last-exception #f)
(define (set-last-exception! e)
  (set! last-exception e))

(let ((original (uncaught-exception-handler)))
  (uncaught-exception-handler (λ (e)
                                (when (exn? e)
                                  (set-last-exception! e)
                                  (original e)))))

(define (debug)
  (browse (cons (exn-message last-exception)
                (exn-call-stack last-exception))))

(define (debug/dagger)
  (dagger:browse
   (current-call-stack)))
#|   (reverse (cons (exn-message last-exception)
                  (exn-call-stack last-exception)))))|#

