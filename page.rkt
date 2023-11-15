#lang racket/base

(require 2htdp/image)

(provide (struct-out page)
         render-page)

(struct page (image text frame-color text-color))

(define width 640)
(define height 480)

(define (render-page page text)
  (define frame-color (page-frame-color page))
  (define text-color (page-text-color page))
  (define image (page-image page))
  (overlay/align "center" "bottom"
                 (render-text-box text frame-color text-color)
                 (overlay/align "center" "top"
                                image (rectangle width height 'solid 'black))))

(define (render-text-box str frame-color text-color)
  (define box-height (/ height 3))
  (define inset 20)
  (define border 5)
  (define sum-inset (+ inset border))
  (define frame
    (overlay
     (rectangle (- width (* 2 border)) (- box-height (* 2 border))
                'solid 'black)
     (rectangle width box-height 'solid frame-color)))
  (define text-img (text str 24 text-color))
  (overlay/align/offset
   "left" "top"
   text-img
   (- sum-inset) (- sum-inset)
   frame))
