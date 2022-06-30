#lang racket/gui
(require mrlib/hierlist)

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


; very temp
(define-syntax-rule
  (preds->string v (p s) ...)
  (cond
    [(p v) s]
    ...
    [else "something else"]))

(define (describe-obj obj)
  (format "~v [~a]" obj
          (preds->string
           obj
           (number? "Number")
           (string? "String")
           (struct? "Struct")
           (list? "List"))))

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
      (if (list? cur)
          ; XXX explode structs
          (let ([cur-elem (new-named-list parent (describe-obj cur))])
            (for-each (λ (child)
                        (init-sexp child cur-elem))
                      cur)
            cur-elem)
          (new-named-item parent (describe-obj cur))))
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
  (define f (new frame% [label "dagger"] [width 400] [height 400]))
  (define buttons (new horizontal-panel% [parent f] [stretchable-height #f]))
  (define list-top (new sexp-list% [parent f] [sexp obj]))
  (new button% [parent buttons] [label "Expand all"]
       [callback (lambda (b e)
                   (send list-top expand-all))])
  (new button% [parent buttons] [label "Collapse all"]
       [callback (lambda (b e)
                   (send list-top collapse-all))])
  (send f show #t))
  
        
;(browse '(1 2 (3 (4 "five")) 6))