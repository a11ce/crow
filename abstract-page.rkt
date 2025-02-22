#lang racket/base

(require "page.rkt")

(provide (struct-out abstract-complete-page)
         eval-abstract-complete-page)

(struct abstract-complete-page (name directives λpage) #:transparent)

(define (eval-abstract-complete-page page story-state)
  ((abstract-complete-page-λpage page) story-state))
