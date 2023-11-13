#lang racket/base

(require 2htdp/image
         2htdp/universe
         racket/generator)

(define a11ce (color 161 28 224))

(define (prefix str idx)
  (substring str 0 (min idx (string-length str))))

(define (printer-gen str)
  (generator ()
    (for ([idx (in-naturals)])
      (yield (text (prefix str idx) 24 a11ce)))))
        
(struct state (text-gen))

(define (init)
  (state (printer-gen "meow meow meow meow meow meow meow meow\nmeow")))

(define width 640)
(define height 480)

(define (render-text-box text-img)
  (define box-height (/ height 3))
  (define inset 20)
  (define border 5)
  (define sum-inset (+ inset border))
  (define frame
    (overlay
     (rectangle (- width (* 2 border)) (- box-height (* 2 border))
                'solid 'black)
     (rectangle width box-height 'solid a11ce)))
  (overlay/align/offset
   "left" "top"
   text-img
   (- sum-inset) (- sum-inset)
   frame))
  
(define (render s)
  (overlay/align
   "center" "bottom"
   (render-text-box ((state-text-gen s)))
   (rectangle width height 'solid 'black)))

(big-bang (init)
  [to-draw render]
  [on-tick values 1/15])
