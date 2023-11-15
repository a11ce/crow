#lang racket/base

(require "page.rkt")

(provide (struct-out choice-page)
         (struct-out choice-opt)
         (struct-out selection)
         render-choice-page)

(struct selection (n))

(struct choice-opt (label pages))
(struct choice-page page (opts))

(define (render-choice-page page text)
  (render-page page text))
