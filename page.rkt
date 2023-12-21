#lang racket/base

(require 2htdp/image
         "text-box.rkt")

(provide (struct-out page)
         render-page
         put-page-image-pinhole)

(struct page (image text frame-color text-color))

(define width 640)
(define height 480)

(define (render-page page text)
  (define frame-color (page-frame-color page))
  (define text-color (page-text-color page))
  (define image (page-image page))
  (overlay/align "center" "bottom"
                 (render-main-text-box text frame-color text-color)
                 (overlay/align "center" "top"
                                image (rectangle width height 'solid 'black))))

(define (put-page-image-pinhole page-img)
  (put-pinhole (/ width 2) (/ height 3) page-img))

(define (render-main-text-box text frame-color text-color)
  (render-text-box text frame-color text-color width (/ height 3) 20 5))
