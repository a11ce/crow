#lang racket/base

(require (except-in 2htdp-raven/image text)
         "text.rkt")

(provide render-text-box
         render-centered-text-box)

(define (render-text-box str frame-color text-color
                         width height
                         inset border)
  (define sum-inset (+ inset border))
  (define text-img (text str 32 text-color))
  (define frame (render-frame frame-color width height border))
  (overlay/align/offset
   "left" "top"
   text-img
   (- sum-inset) (- sum-inset)
   frame))

(define (render-centered-text-box str frame-color text-color
                                  width height
                                  border)
  (define text-img (text str 32 text-color))
  (define frame (render-frame frame-color width height border))
  (overlay text-img frame))

(define (render-frame frame-color width height border)
  (overlay
   (rectangle (- width (* 2 border)) (- height (* 2 border))
              'solid 'black)
   (rectangle width height 'solid frame-color)))
