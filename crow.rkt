#lang racket/base

(require (except-in 2htdp-raven/universe state)
         "vn-gen.rkt"
         "input.rkt"
         "test-data.rkt")

(define (init)
  (state (vn-gen test-pages)
         page-handle-key
         page-handle-mouse))

(define (render s)
  (define ctx*img ((state-text-gen s) 'render))
  (set-state-on-mouse! s (vn-ctx-on-mouse ctx*img))
  (set-state-on-key! s (vn-ctx-on-key ctx*img))
  (vn-ctx-image ctx*img))

(define (tick s)
  ((state-text-gen s) 'tick)
  s)

(define (dispatch-handle-key s k)
  (define on-key (state-on-key s))
  (on-key s k))

(define (dispatch-handle-mouse s x y evt)
  (define on-mouse (state-on-mouse s))
  (on-mouse s x y evt))

(big-bang (init)
  [to-draw render]
  [on-key dispatch-handle-key]
  [on-mouse dispatch-handle-mouse]
  [on-tick tick 1/15])
