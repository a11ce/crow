#lang racket/base

(require 2htdp-raven/image
         "page.rkt"
         "text-box.rkt")

(provide (struct-out choice-page)
         (struct-out choice-opt)
         render-choice-page
         choice-button-bboxes
         in-bbox?
         bbox-tag)

(struct choice-opt (label pages) #:transparent)
(struct choice-page page (opts) #:transparent)

(define (render-choice-page page text hovered)
  (define page-base
    (put-page-image-pinhole (render-page page text)))
  (define options-img
    (apply above
           (cons
            empty-image ; TODO safe-above
            (for/list ([opt (choice-page-opts page)]
                       [idx (length (choice-page-opts page))])
              (above
               (rectangle 1 10 'solid (color 0 0 0 0))
               (render-choice-button (choice-opt-label opt)
                                     (if (equal? idx hovered)
                                         (color 39 193 155)
                                         (page-frame-color page))
                                     (page-text-color page))
               (rectangle 1 10 'solid (color 0 0 0 0)))))))
  (clear-pinhole (overlay/pinhole
                  options-img
                  page-base)))

; TODO param
(define width 640)
(define height 480)

(define (render-choice-button text frame-color text-color)  
  (render-centered-text-box text frame-color text-color
                            (* 1/3 width)
                            (* 1/10 height)
                            5))

(struct bbox (tag tl br) #:transparent)
(struct pt (x y) #:transparent)
(define (pt+y a n)
  (pt (pt-x a)
      (+ (pt-y a) n)))

(define (bbox-tl-x b) (pt-x (bbox-tl b)))
(define (bbox-tl-y b) (pt-y (bbox-tl b)))
(define (bbox-br-x b) (pt-x (bbox-br b)))
(define (bbox-br-y b) (pt-y (bbox-br b)))

(define (in-bbox? b x y)
  (and (<= (bbox-tl-x b) x (bbox-br-x b))
       (<= (bbox-tl-y b) y (bbox-br-y b))))
  
(define (choice-button-bbox-at tag x y)
  (define half-width ( * 1/2 1/3 width))
  (define half-height (* 1/2 1/10 height))
  (bbox tag
        (pt (- x half-width)
            (- y half-height))
        (pt (+ x half-width)
            (+ y half-height))))
 
(define (choice-button-bboxes n)
  (if (even? n)
      (choice-button-bboxes-even n)
      (choice-button-bboxes-odd n)))

(define (choice-button-bboxes-even n)
  (define center-x (/ width 2))
  (define center-y (/ height 3))
  (define center-tag (floor (/ n 2)))
  (define dist (+ 10 (* 1/2 1/10 height)))
  (for/fold
   ([acc '()])
   ([idx (in-range (/ n 2))])
    (append (list (choice-button-bbox-at
                   (+ center-tag idx)
                   center-x (+ center-y idx dist))
                  (choice-button-bbox-at
                   (- center-tag idx 1)
                   center-x (+ center-y (* -1 (add1 idx) dist))))
            acc)))

(define (choice-button-bboxes-odd n)
  (define center-x (/ width 2))
  (define center-y (/ height 3))
  (define center-tag (floor (/ n 2)))
  (define dist (+ 10 (* 1/2 1/10 height)))
  (cons (choice-button-bbox-at center-tag center-x center-y)
        (for/fold
         ([acc '()])
         ([idx (in-range (/ n 2))])
          (append (list (choice-button-bbox-at
                         (+ center-tag idx)
                         center-x (+ center-y (* (add1 idx) dist)))
                        (choice-button-bbox-at
                         (- center-tag idx)
                         center-x (+ center-y (* -1 (add1 idx) dist))))
                  acc))))
  
;;;;
#|
(define blue (color 39 193 155))
(define purple (color 161 28 224))
(define red (color 211 56 85))

(define ex-page
  (render-choice-page
   (choice-page
    (bitmap/file "train.png") "choose awoo(1) or arf(2)?" purple blue
    (list (choice-opt "wolf"
                      (list (page (bitmap/file "trees.png") "awoo awoo" purple blue)
                            (page (bitmap/file "train.png") "awooooooooooooooooooooooooooo" red blue)))
          (choice-opt "dog"
                      (list (page (bitmap/file "train.png") "arf arf arf" purple blue)))
          (choice-opt "meow"
                      (list))))
   "pick"))

(define (debug-render-choice-bboxes page-img n)
  (define bboxes (choice-button-bboxes n))
  (for/fold
   ([img page-img])
   ([bbox bboxes]
    [color '(red green blue)])
    (printf "~a: ~a ~n" (bbox-tag bbox) color)
    (add-line img
              (bbox-tl-x bbox) (bbox-tl-y bbox)
              (bbox-br-x bbox) (bbox-br-y bbox) color)))

(debug-render-choice-bboxes ex-page 3)
|#
