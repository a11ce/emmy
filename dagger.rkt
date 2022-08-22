#lang racket/base
(require racket/gui/base
         racket/class
         mrlib/hierlist
         "struct.rkt"
         "common-types.rkt")

(provide browse code-mode)

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
; ???? maybe fine
(struct label (text))

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
    [(symbol? obj) "Symbol"]
    [(proc*? obj)  (proc*-typestring obj)]
    [(s-name? obj) (s-name obj)]
    [(procedure? obj) "Built-in procedure (not yet augmented)"]
    [else "???"]))

(define (listize obj)
  (cond
    [(list? obj)   obj]
    [(proc*? obj) #f]
    [(s-name? obj) (struct->named-fields obj)]
    [(named-field? obj) (listize (named-field-val obj))]
    [else #f]))

(define (describe-eval obj)
  (format "~a -> ~a"
          obj
          (with-handlers ([exn:fail?
                           (λ (e) "error!")])
            (describe (eval obj)))))

(define (desc*?listize obj)
  (cond
    [(label? obj) (values (label-text obj) #f)]
    [(code-mode)
     (cond
       [(list? obj)
        (values (describe-eval obj)
                (if (equal? (car obj) 'quote)
                    #f obj))]
       [else (values (describe (eval obj))
                     (listize obj))])]
    [(call-ctx? obj) (interpret-call-ctx obj)]
    [else (values (describe obj) (listize obj))]))

(define (ctx-call-rep ctx)
  (format "(~a~a)"
          (call-ctx-proc ctx)
          (apply string-append
                 (map (λ (arg) (format " ~a" arg))
                      (call-ctx-args ctx)))))

(define (interpret-call-ctx obj)
  (values (format "Within ~a" (ctx-call-rep obj))
          (append
           (list (call-ctx-proc obj))
           (call-ctx-args obj)
           (if (flat-call-stack) '()
               (list
                (or (call-ctx-outer-call obj)
                    (label "{Top-level call}")))))))
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
      (if listized
          (let ([cur-elem (new-named-list parent desc)])
            (for-each (λ (child)
                        (init-sexp child cur-elem))
                      listized)
            cur-elem)
          (new-named-item parent desc)))
    (super-new)
    (define list-top (init-sexp sexp this))
    (define/public (rename-top new-name)
      (send list-top set-text new-name))
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

(define (browse obj [err #f] [blocking #f])
  (define f (new frame% [label "dagger"] [width 800] [height 800]))
  (send f show #t)
  (define buttons (new horizontal-panel% [parent f] [stretchable-height #f]))
  (define list-top (new sexp-list% [parent f] [sexp obj]))
  (when err
    (send list-top rename-top
          (if (flat-call-stack)
              (format "~a~n" err)
              (format "~a~n~nWithin ~a"
                      err (ctx-call-rep obj)))))
  (new button% [parent buttons] [label "Expand all"]
       [callback (lambda (b e)
                   (send list-top expand-all))])
  (new button% [parent buttons] [label "Collapse all"]
       [callback (lambda (b e)
                   (send list-top collapse-all))])
  (if err
      (send (car (send list-top get-items)) open)
      (send list-top expand-all))
  (when blocking
    (define blocker (make-channel))
    (new button% [parent buttons] [label "Continue program"]
         [callback (lambda (b e)
                     (channel-put blocker 'meow))])
    (yield (thread (λ ()
                     (sync blocker)
                     (send f show #f))))))