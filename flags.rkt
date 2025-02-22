#lang racket/base

(require racket/contract
         racket/set)

(provide make-flagset
         flagset-add
         has-flag?)

(define make-flagset set)

(define flagset? generic-set?)

(define/contract (flagset-add flagset flag)
  (-> flagset? symbol? flagset?)
  (set-add flagset flag))

(define/contract (has-flag? flagset flag)
  (-> flagset? symbol? boolean?)
  (set-member? flagset flag))
