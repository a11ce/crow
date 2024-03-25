#lang racket/base

(require (except-in 2htdp-raven/universe state)
         racket/contract
         "vn-gen.rkt"
         "input.rkt"
         "test-data.rkt")

(define (init)
  (crow-state (vn-gen test-pages)
              page-handle-key
              page-handle-mouse))

(define (render s)
  (define ctx*img ((crow-state-text-gen s) 'render))
  (set-crow-state-on-mouse! s (vn-ctx-on-mouse ctx*img))
  (set-crow-state-on-key! s (vn-ctx-on-key ctx*img))
  (vn-ctx-image ctx*img))

(define (tick s)
  ((crow-state-text-gen s) 'tick)
  s)

(define/contract (dispatch-handle-key s k)
  handle-key/c
  (define on-key (crow-state-on-key s))
  (on-key s k))

(define/contract (dispatch-handle-mouse s x y evt)
  handle-mouse/c
  (define on-mouse (crow-state-on-mouse s))
  (on-mouse s x y evt))

(big-bang (init)
  [to-draw render]
  [on-key dispatch-handle-key]
  [on-mouse dispatch-handle-mouse]
  [on-tick tick 1/15])
