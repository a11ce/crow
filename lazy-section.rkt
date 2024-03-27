#lang racket/base

(require "page.rkt")

(provide (struct-out lazy-section)
         eval-lazy-section)

; TODO what are the inputs here exactly
(struct lazy-section (λimage λtext λframe-color λtext-color))

(define (eval-lazy-section sec . args)
  (text->pages (apply (lazy-section-λimage sec) args)
               (apply (lazy-section-λtext sec) args)
               (apply (lazy-section-λframe-color sec) args)
               (apply (lazy-section-λtext-color sec) args)))
