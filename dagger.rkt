#lang racket/base
(require racket/gui/base
         racket/class
         mrlib/hierlist
         "struct.rkt"
         "procedure.rkt")

(provide browse)

; from https://docs.racket-lang.org/mrlib/Hierarchical_List_Control.html
(define set-text-mixin
  (mixin (hierarchical-list-item<%>)
    ((interface () set-text))
    (inherit get-editor)
    (super-new)
    ; set-text: this sets the label of the item
    (define/public (set-text str)
      (define t (get-editor)) ; a text% object
      (send t erase)
      (send t insert str))))

;;; description/normalization

; type is the defined type, not the observed one
(struct named-field (name type val))

(define (struct->named-fields obj)
  (for/list ([name (s-fields obj)]
             [type (s-types obj)]
             [val (s-vals obj)])
    (named-field name type val)))
; XXX

(define (describe obj)
  (if (named-field? obj)
      (format "~v : [~a : ~a]"
              (named-field-val obj)
              (named-field-name obj)
              (named-field-type obj))
      (format "~v : ~a" obj (describe-type obj))))

; this is not good enough
(define (describe-type obj)
  (cond
    [(number? obj) "Number"]
    [(string? obj) "String"]
    [(list? obj)   "List"]
    [(proc*? obj)  (proc*-typestring obj)]
    [(struct? obj) (s-name obj)]
    [else "???"]))

(define (listize obj)
  (cond
    [(list? obj)   obj]
    [(proc*? obj) #f]
    [(struct? obj) (struct->named-fields obj)]
    [(named-field? obj) (listize (named-field-val obj))]
    [else #f]))

(define (desc*?listize obj)
  (values (describe obj) (listize obj)))
    
;;;

(define sexp-list%
  (class hierarchical-list% (init [sexp #f])
    (define (new-named-list parent name)
      (let ([i (send parent new-list set-text-mixin)])
        (send i set-text name)
        i))
    (define (new-named-item parent name)
      (let ([i (send parent new-item set-text-mixin)])
        (send i set-text name)
        i))
    (define (init-sexp cur parent)
      (define-values (desc listized) (desc*?listize cur))
      ;  (define desc (describe-obj cur))
      (if listized
          (let ([cur-elem (new-named-list parent desc)])
            (for-each (λ (child)
                        (init-sexp child cur-elem))
                      listized)
            cur-elem)
          (new-named-item parent desc)))
    (super-new)
    (define list-top (init-sexp sexp this))
    (define/public (expand-all [elem list-top])
      (when (is-a? elem hierarchical-list-compound-item<%>)
        (begin (send elem open)
               (for-each (λ (child) (expand-all child))
                         (send elem get-items)))))
    (define/public (collapse-all [elem list-top])
      (when (is-a? elem hierarchical-list-compound-item<%>)
        (begin (for-each (λ (child) (collapse-all child))
                         (send elem get-items))
               (send elem close))))))

(define (browse obj)
  (define f (new frame% [label "dagger"] [width 800] [height 800]))
  (define buttons (new horizontal-panel% [parent f] [stretchable-height #f]))
  (define list-top (new sexp-list% [parent f] [sexp obj]))
  (new button% [parent buttons] [label "Expand all"]
       [callback (lambda (b e)
                   (send list-top expand-all))])
  (new button% [parent buttons] [label "Collapse all"]
       [callback (lambda (b e)
                   (send list-top collapse-all))])
  (send list-top expand-all)
  (send f show #t))

(define-syntax-rule
  (browse-code obj)
  (browse (quote obj)))
