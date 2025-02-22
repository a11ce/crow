#lang racket/base

(require racket/contract
         racket/match
         (only-in "oat-parse.rkt"
                  directive?
                  directive-type
                  directive-args)
         "flags.rkt")

(provide make-run-state
         run-state?
         apply-directives
         run-state-has-flag?)

(struct run-state (flags) #:transparent)

(define (run-state-has-flag? state flag)
  (has-flag? (run-state-flags state) flag))

(define/contract (apply-directives state dirs)
  (-> run-state? list? run-state?)
  (for/fold ([state state])
            ([dir dirs])
    (apply-directive state dir)))

(define/contract (apply-directive state dir)
  (-> run-state? directive? run-state?)
  (match (directive-type dir)
    ; TODO cring
    ['set-flag
     (run-state (flagset-add (run-state-flags state)
                             (car (directive-args dir))))]
    [else
     (error "unknown directive type " dir)]))

(define (make-run-state)
  (run-state (make-flagset)))
