#lang typed/racket/base

(provide (struct-out call-ctx))

; needed by struct and dagger,
; this avoids circular imports
(struct call-ctx
  ([proc : Any]
   [args : (Listof Any)]
   [outer-call : (U call-ctx False)])
  #:transparent
  #:mutable)