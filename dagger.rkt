#lang racket/gui
(require mrlib/hierlist
         racket/struct
         struct-plus-plus
         "struct-props-typed.rkt")

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

(define (desc*?listize obj)
  (cond
    [(number? obj) (values (format "~v : Number" obj) #f)]
    [(string? obj) (values (format "~v : String" obj) #f)]
    [(list? obj)   (values (format "~v : List" obj)  obj)]
    [(struct? obj) (values (format "~v : ~a" obj (s-name obj)) (s-vals obj))]
    [else (values "Other type" #f)]))
    
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
  (define f (new frame% [label "dagger"] [width 400] [height 400]))
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
