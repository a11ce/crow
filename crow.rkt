#lang racket/base

(require 2htdp/image
         2htdp/universe
         racket/generator)

(define a11ce (color 161 28 224))

(define (prefix str idx)
  (substring str 0 (min idx (string-length str))))

(define (vn-gen render pages)
  (generator (_)
    (for/fold ([pages pages]
               [idx 0])
              ([_ (in-naturals)])
      (define page (car pages))
      (define page-length (string-length page))
      (define command (yield (render (prefix page idx))))
      (case command
        [(tick) (values pages (add1 idx))]
        [(advance) (if (>= idx (string-length page))
                       (values (cdr pages) 0)
                       (values pages page-length))]
        [(wait) (values pages idx)]
        [else (error "unknown vn-gen command" command)]))))

(struct state (text-gen))

(define (init)
  (state (vn-gen (λ (str) (text str 24 a11ce))
                 '("meow meow meow meow meow meow meow meow\nmeow"
                   "bark bark bark"))))

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

(define train-img (bitmap/file "train.png"))
; stark 8x8

(define (render s)
  (overlay/align "center" "bottom"
                 (render-text-box ((state-text-gen s) 'wait))
                 (overlay/align
                  "center" "top"
                  train-img
                  (rectangle width height 'solid 'black))))

(define (tick s)
  ((state-text-gen s) 'tick)
  s)

(define (handle-key s key)
  (when (key=? key " ")
    ((state-text-gen s) 'advance))
  s)


(big-bang (init)
  [to-draw render]
  [on-key handle-key]
  [on-tick tick 1/15])
