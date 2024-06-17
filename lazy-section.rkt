#lang racket/base

(require "page.rkt")

(provide (struct-out lazy-section)
         (struct-out lazy-complete-page)
         eval-lazy-section
         eval-lazy-complete-page)

(struct lazy-section (λimage λtext λframe-color λtext-color))

(define (eval-lazy-section sec . args)
  (text->pages (apply (lazy-section-λimage sec) args)
               (apply (lazy-section-λtext sec) args)
               (apply (lazy-section-λframe-color sec) args)
               (apply (lazy-section-λtext-color sec) args)))

(struct lazy-complete-page (λpage))

(define (eval-lazy-complete-page page . flagset)
  (apply (lazy-complete-page-λpage page) flagset))
