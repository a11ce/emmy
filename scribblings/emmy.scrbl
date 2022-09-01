#lang scribble/manual

@(require scribble/example
          (for-label emmy))

@(define-syntax-rule (id dat)
   (racketvarfont (symbol->string 'dat)))

@(define-syntax-rule (rid dat)
   (racketid dat))

@(define emmy-eval
   (make-base-eval))
@examples[#:eval emmy-eval
          #:hidden
          (require "../main.rkt")]

@(define-syntax-rule (ex . args)
   (examples #:eval emmy-eval #:label #f
             . args))
@;(define-syntax-rule (ex . args) (racketblock . args))

@title{Emmy}

@defmodulelang[emmy]

Emmy is a new student language for 111.
Everything is subject to change, especially the documentation.

The evaluated examples in this doc are a bit weird.
Things don't print correctly, and there are redlinks where they shouldn't be.
Run them in DrRacket instead.

Report any problems with Emmy
@(link "https://github.com/a11ce/emmy/issues/new/choose" "here").
Good error messages are one of Emmy's goals. If you see an unhelpful one,
please report it. Questions, suggestions, and contributions are also welcome.

@section{Definitions}

@defform*[((define name expr)
           (define (name arg ...) etc ...))]{

 The first form defines @id[name] as the result of @id[expr].
 The second form converts directly to:

 @racketblock[
 (define name
   (lambda (arg ...)
     etc ...))
 ]

}

@section{Types}

All user-defined types must be capitalized.

@defform[(define-type name t)]{

 Defines @id[name] as type @id[t], where @id[t] is a
 @seclink["types" #:doc '(lib "typed-racket/scribblings/ts-guide.scrbl")
          "Typed Racket type"] or special top-level type constructor.
}

@subsection{Special top-level type constructors.}

Certain type constructors can only be used at the top level of a type
definition.


@defform[#:id [Struct #'Struct] #:literals (: define-type)
         (define-type Name (Struct [field : type]
                                   ...
                                   ))]{

 Defines @id[Name] as a structure type. The following names are defined:
 @itemlist[

 @item{@id[Name] is defined as a type, corresponding to instances
   of the struct.}

 @item{@id[Name] is also defined as a procedure that takes an argument for each
   field and returns a new instance of the struct.}

 @item{@id[Name]@rid[?] is defined as a predicate that returns @racket[#t]
   given an instance of the struct and @racket[#f] for any other value.}

 @item{@id[Name]@rid[-]@id[field], for each
   @id[field], a procedure that takes an instance of the struct and returns the
   value for the given field.}

 ]

 @ex[
 (define-type Vector2 (Struct
                       [x : Number]
                       [y : Number]))

 (define (vector2-+ [a : Vector2] [b : Vector2]) -> Vector2
   "Adds two Vector2s"
   (Vector2 (+ (Vector2-x a)
               (Vector2-x b))
            (+ (Vector2-y a)
               (Vector2-y b))))

 (define V1 (Vector2 1 2))

 (vector2-+ V1
            (Vector2 3 4))
 ]

 Struct instances display as
 @rid[<]@id[Name] @id[field]@rid[=]@id[val] @id[...]@rid[>].
}

@defform[#:id [NamedOptions #'NamedOptions] #:literals (define-type)
         (define-type Name (NamedOptions opt ...
                                         ))]{

 Defines each @id[opt] as a singleton type, and @id[Name]
 as their union.

 @ex[
 (define-type Image-Mode (NamedOptions Solid Outline))
 ]
}

@section{Procedures}

@defform/subs[#:literals (: ->)
              (lambda maybe-name (arg ...) -> return-type
                maybe-desc
                body ...+)
              [(arg [arg-name : type])
               (maybe-name (code:line)
                           name-string)
               (maybe-desc (code:line)
                           desc-string)]]{

 Creates a procedure. It accepts an argument for each @id[arg] (as long as it
 is correctly typed), and returns the value of the final expression in
 @id[body], which must be of type @id[return-type].

 Procedures may be given names and descriptions, and will display as
 @rid[\{]@id[name]@rid[\;] @id[desc]@rid[\}]. Procedures at the
 top level of a @racket[define] are named accordingly.

 Single capital letters serve as 'type variables'
 and can be used for generic procedures:

 @racketblock[
 (define second
   (λ ([lst : (Listof A)]) -> A
     "Gets the second element of a homogeneous list"
     (code:comment @#,elem{first/rest don't exist yet})
     (car (cdr lst))))
 ]

 Note that this version of @rid[second] requires all elements of the given
 list to be the same type. Replacing @rid[A] with @racket[Any] would remove
 this restriction.

}

@section{The Object Browser}

The object browser will be launched whenever a runtime(ish) error occurs,
examining the stack at the time of the error.

@defproc[(browse [v Any]) Void]{

 Opens @id[v] in the object browser. This doesn't pause the program,
 so be careful calling it in a loop!
}

@defproc[(browse-stack-here) Void]{

 Shows the current call stack in the object browser.
 Click "Continue program" to unpause.
}

@defproc[(break-at [v Any]) Void]{
 @italic{Not implemented}

 Opens @id[v] and the current call stack in the object browser.
 Click "Continue program" to unpause.
}


@section{Other Syntax}

@defform[#:id :
         [n : t]]{

 Denotes that @id[n] is defined to have type @id[t].
}


@defform[#:id ->
         (code:line -> t)]{

 Denotes that a procedure returns a value of type @id[t].
}

@defform[#:id ...
         (code:line f ...)]{

 Denotes that @id[f] may repeat any number of times.
 Used only in documentation.
}

@defform[#:id λ
 (code:line λ)]{

 Alias of @racket[lambda].
}

@section{The Image Library}

It doesn't exist.
(But look at @rid[test/images.rkt] for what it could look like.)

@section{Internals}

Emmy has some features for autograding and library development.
In the future, these may either be solidified into the main language
or moved to a sub-language.

- @rid[proc*-typestring] gets the typestring (as seen in Dagger) of a procedure.

- @rid[proc*-arg-types] and @rid[proc*-return-type] get type info
as a symbol or nested list of symbols. Currently, infix vs prefix @rid[->]s
will keep their given shape, which needs to be fixed in order to be suitable for
autograding.

- Explicit type variables can be given with @racket[#:forall (V ...)],
before the name/args in shortcut notation or anonymous lambdas. If provided,
automatic type variables will not be discovered.

- Each instance of a struct carries information about its type with
@rid[s-name], @rid[s-fields], and @rid[s-types].
