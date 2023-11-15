#lang racket/base

(require 2htdp/universe
         "vn-gen.rkt"
         "choice-page.rkt"
         "test-data.rkt")

(struct state (text-gen))

(define (init)
  (state (vn-gen test-pages)))

(define (render s)
  ((state-text-gen s) 'poll))

(define (tick s)
  ((state-text-gen s) 'tick)
  s)

(define (handle-key s key)
  (when (key=? key " ")
    ((state-text-gen s) 'advance))
  (when (key=? key "1")
    ((state-text-gen s) (selection 1)))
  (when (key=? key "2")
    ((state-text-gen s) (selection 2)))
  s)

(big-bang (init)
  [to-draw render]
  [on-key handle-key]
  [on-tick tick 1/15])
