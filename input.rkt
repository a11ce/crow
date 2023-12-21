#lang racket/base

(require 2htdp/universe
         "choice-page.rkt")

(provide (struct-out state)
         page-handle-key
         page-handle-mouse
         make-choice-page-handle-mouse)

(struct state (text-gen on-key on-mouse) #:mutable)

(define (page-handle-key s key)
  (when (key=? key " ")
    ((state-text-gen s) 'advance))
  s)

(define (page-handle-mouse s x y evt)
  s)

(define (make-choice-page-handle-mouse n)
  (define bboxes (choice-button-bboxes n))
  (lambda (s x y evt)
    (when (equal? evt "button-down")
      (for ([bbox bboxes]
            #:when (in-bbox? bbox x y))
        ((state-text-gen s) `(select ,(bbox-tag bbox)))))
    s))
