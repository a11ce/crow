#lang racket/base

(require 2htdp-raven/universe
         racket/contract
         "choice-page.rkt")

(provide (struct-out crow-state)
         page-handle-key
         page-handle-mouse
         make-choice-page-handle-mouse
         handle-mouse/c handle-key/c)

(struct crow-state (text-gen on-key on-mouse) #:mutable #:transparent)

(define handle-mouse/c
  (-> crow-state? integer? integer? mouse-event? crow-state?))

(define handle-key/c
  (-> crow-state? key-event? crow-state?))

(define/contract (page-handle-key s key)
  handle-key/c
  (when (key=? key " ")
    ((crow-state-text-gen s) 'advance))
  s)

(define/contract (page-handle-mouse s x y evt)
  handle-mouse/c
  s)

(define/contract (make-choice-page-handle-mouse n)
  (-> integer? handle-mouse/c)
  (define bboxes (choice-button-bboxes n))
  (define/contract (choice-page-handle-mouse s x y evt)
    handle-mouse/c
    (unless
        (for/first ([bbox bboxes]
                    #:when (in-bbox? bbox x y))
          (case evt
            [("move")
             ((crow-state-text-gen s) `(hover ,(bbox-tag bbox)))]
            [("button-down")
             ((crow-state-text-gen s) `(select ,(bbox-tag bbox)))]))
      ((crow-state-text-gen s) '(hover #f)))
    s)
  choice-page-handle-mouse)
