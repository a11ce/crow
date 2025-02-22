#lang racket/base

(require "page.rkt")

(provide (struct-out abstract-section)
         (struct-out abstract-complete-page)
         eval-abstract-section
         eval-abstract-complete-page)

(struct abstract-section (λimage λtext λframe-color λtext-color))

(define (eval-abstract-section sec . args)
  (text->pages (apply (abstract-section-λimage sec) args)
               (apply (abstract-section-λtext sec) args)
               (apply (abstract-section-λframe-color sec) args)
               (apply (abstract-section-λtext-color sec) args)))

(struct abstract-complete-page (name directives λpage) #:transparent)

(define (eval-abstract-complete-page page story-state)
  ((abstract-complete-page-λpage page) story-state))
