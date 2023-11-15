#lang racket/base

(require 2htdp/image
         2htdp/universe
         racket/generator
         "page.rkt"
         "test-data.rkt")

(define (prefix str idx)
  (substring str 0 (min idx (string-length str))))

(define (vn-gen pages)
  (generator (_)
    (for/fold ([pages pages]
               [idx 0])
              ([_ (in-naturals)])
      (define page (car pages))
      (define text (page-text page))
      (define page-length (string-length text))
      (define command (yield (render-page page (prefix text idx))))
      (case command
        [(tick) (values pages (add1 idx))]
        [(advance) (if (>= idx (string-length text))
                       (values (cdr pages) 0)
                       (values pages page-length))]
        [(wait) (values pages idx)]
        [else (error "unknown vn-gen command" command)]))))

(struct state (text-gen))

(define (init)
  (state (vn-gen test-pages)))

(define (render s)
  ((state-text-gen s) 'wait))

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
