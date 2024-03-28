#lang racket/base

(require racket/contract
         racket/set)

(provide (struct-out flag-set-point)
         make-flagset
         flagset-add
         flag-cond)

(struct flag-set-point (flag))

(define make-flagset set)

(define flagset? generic-set?)

(define/contract (flagset-add flagset flag)
  (-> flagset? symbol? flagset?)
  (set-add flagset flag))

(define/contract (has-flag? flagset flag)
  (-> flagset? symbol? boolean?)
  (set-member? flagset flag))

(define/contract (flag-cond flag then else)
  (-> symbol? any/c any/c (-> flagset? any/c))
  (lambda (flagset)
    (if (has-flag? flagset flag)
        then else)))
