#lang racket/base

(require (except-in 2htdp/image text)
         racket/match
         memo
         "font.rkt")

(provide text)

; XXX wrapping
(define (text string size color)
  (define glyphs
    (map (λ (c) (draw-char c size color))
         (string->list string)))
  (cond
    [(null? glyphs) empty-image]
    [(null? (cdr glyphs)) (car glyphs)]
    [else (apply beside glyphs)]))

(define (draw-char char size color)
  (color-glyph (scale-glyph (hash-ref the-font char) size) color))

(define/memoize (scale-glyph glyph size)
  (scale (/ size (image-height glyph)) glyph))

(define transparent (color 0 0 0 0))
(define/memoize (color-glyph glyph to-color)
  (define pix (image->color-list glyph))
  (define new-pix
    (map (match-lambda
           [(color x y z 255) to-color]
           [_ transparent])
         pix))
  (color-list->bitmap new-pix
                      (image-width glyph)
                      (image-height glyph)))
