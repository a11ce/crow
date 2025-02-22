#lang racket/base

(require "page.rkt")

(provide (struct-out abstract-page)
         eval-abstract-page)

(struct abstract-page (name directives λpage) #:transparent)

(define (eval-abstract-page page story-state)
  ((abstract-page-λpage page) story-state))
