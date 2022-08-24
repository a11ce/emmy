#lang emmy

(require (for-syntax racket/base syntax/parse))

(define-syntax (define-compat-enum stx)
  (syntax-parse stx
    ([_ name:id ([new-name:id opt:expr] ...)]
     #'(begin
         (define new-name opt) ...
         (define-type name (U opt ...))))))

(define-compat-enum Image-Mode-Consts
  ([Solid 'solid]
   [Outline 'outline]))
(define-type Image-Mode (U Byte Image-Mode-Consts))

(define-type Color (Struct [r : Byte]
                           [g : Byte]
                           [b : Byte]))

(define red (Color 255 0 0))
(define green (Color 0 255 0))
(define blue (Color 0 0 255))
(define white (Color 255 255 255))
(define black (Color 0 0 0))

(require/typed 2htdp/image
               [#:opaque Image image?]
               [#:opaque %Color color?]
               [(make-color %make-color) (-> Byte Byte Byte %Color)]
               [(circle %circle) (-> Number Image-Mode %Color Image)]
               [(rectangle %rectangle) (-> Number Number Image-Mode %Color Image)]
               [overlay (-> Image * Image)]
               [beside (-> Image * Image)])

(define (color->%color [c : Color]) -> %Color
  (%make-color (Color-r c)
               (Color-g c)
               (Color-b c)))

(define (circle [radius : Number] [mode : Image-Mode] [color : Color])
  -> Image
  "Constructs a circle with the given radius, mode, and color."
  (%circle radius mode (color->%color color)))

(define (rectangle [width : Number] [height : Number] [mode : Image-Mode] [color : Color])
  -> Image
  "Constructs a rectangle with the given width, height, mode, and color."
  (%rectangle width height mode (color->%color color)))

;;;

(define (iterate [combiner : (A A -> A)]
                 [generator : (Integer -> A)]
                 [count : Integer]) -> A
  (for/fold : A
    ([acc (generator 0)])
    ([idx (in-range 1 count)])
    (combiner acc (generator idx))))

(define (iterated-overlay [generator : (Integer -> Image)]
                          [count : Integer]) -> Image
  "does the iterated overlay thing"
  (iterate overlay generator count))

(define (iterated-beside [generator : (Integer -> Image)]
                         [count : Integer]) -> Image
  "does the iterated beside thing"
  (iterate beside generator count))

;;

(define question-1a
  (overlay (circle 10 Outline black)
           (circle 20 Outline black)
           (circle 30 Outline black)
           (circle 40 Outline black)
           (circle 50 Outline black)))

(define question-1b
  (iterated-overlay (λ ([n : Number]) -> Image
                      (circle (* 10 (+ 1 n))
                              Outline black))
                    5))

(define question-2
  (iterated-beside (lambda ([n : Number]) -> Image
                     ;(browse-stack-here)
                     (rectangle (* 10 (+ 1 n))
                                50 Outline black))
                   7))
