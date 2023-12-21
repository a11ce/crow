#lang racket/base

(require 2htdp/image)

(provide render-text-box)

(define (render-text-box str frame-color text-color
                         box-width box-height
                         inset border)
  (define sum-inset (+ inset border))
  (define frame
    (overlay
     (rectangle (- box-width (* 2 border)) (- box-height (* 2 border))
                'solid 'black)
     (rectangle box-width box-height 'solid frame-color)))
  (define text-img (text str 24 text-color))
  (overlay/align/offset
   "left" "top"
   text-img
   (- sum-inset) (- sum-inset)
   frame))
