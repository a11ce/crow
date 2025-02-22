#lang racket/base

(require 2htdp-raven/image
         "text-box.rkt"
         "wrapping.rkt")

(provide (struct-out page)
         (struct-out direct-page)
         blank-page-image
         text->pages
         render-page
         put-page-image-pinhole)

(struct page (image text frame-color text-color) #:transparent)

(struct direct-page page (next))

(define (text->pages text image frame-color text-color)
  (map (λ (c)
         (page image c frame-color text-color))
       (wrap text)))

(define width 640)
(define height 480)

(define blank-page-image empty-image)

(define frame-render-count (make-parameter 0))
(define (inc-frame-count!) (frame-render-count (add1 (frame-render-count))))

(define (render-page page text)
  (inc-frame-count!)
  ;(printf "render ~a~n" (frame-render-count))
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
  (render-text-box text frame-color text-color width (/ height 3) 10 5))
