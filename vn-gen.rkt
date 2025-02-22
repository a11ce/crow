#lang racket/base

(require racket/generator
         racket/match
         racket/contract
         "page.rkt"
         "choice-page.rkt"
         "abstract-page.rkt"
         "run-state.rkt"
         "input.rkt")

(provide vn-gen
         (struct-out vn-ctx))

; TODO more caching probably
(struct vn-ctx (image on-key on-mouse) #:transparent)

(define (vn-gen-command? dat)
  (match dat
    [(or 'advance 'tick 'render)
     #t]
    [(list (or 'hover 'select) cmd)
     #t]
    [else #f]))

(define vn-gen/c
  (-> vn-gen-command? vn-ctx?))

(define (contractualize-vn-gen gen)
  (define/contract (vn-gen . args)
    vn-gen/c
    (apply gen args))
  vn-gen)

(define (prefix str idx)
  (substring str 0 (min idx (string-length str))))

(define (vn-gen first-page)
  (contractualize-vn-gen
   (generator (_)
    (vn-gen-pages first-page (make-run-state)))))

(define (vn-gen-pages start-page init-run-state)
  (let loop ([page start-page]
             [run-state init-run-state])
    (cond
      [(choice-page? page)
       (loop (vn-gen-choice page) run-state)]
      [(abstract-complete-page? page)
       (define next-run-state (apply-directives
                               run-state
                               (abstract-complete-page-directives page)
                               ))
       (define concrete-page (eval-abstract-complete-page page next-run-state))
       (loop concrete-page next-run-state)]
      [else (error "unknown page type" page)])))

(define (vn-gen-page page)
  (define text (page-text page))
  (define page-length (string-length text))
  ; TODO only when advanced
  (define delayed-page-length (+ page-length 5))
  (let loop ([idx 0]
             [to-render? #t])
    (define command (yield (vn-ctx
                            (if to-render?
                                (render-page page (prefix text idx))
                                blank-page-image)
                            page-handle-key
                            page-handle-mouse)))

    (case command
      [(tick) (loop (add1 idx) #f)]
      [(advance) (if (>= idx delayed-page-length)
                     #t
                     (loop (add1 (max idx page-length)) #f))]
      [(render) (loop idx #t)]
      [else (error "unknown vn-gen command" command)])))


(define (vn-gen-abstract-section sec flags)
  (define pages (eval-abstract-section sec flags))
  (vn-gen-pages pages flags))

(define (vn-gen-abstract-complete-page lpage flags)
  (define page (eval-abstract-complete-page lpage flags))
  (vn-gen-pages page flags))

(define (vn-gen-choice page)
  (define text (page-text page))
  (define handle-mouse (make-choice-page-handle-mouse
                        (length (choice-page-opts page))))
  (let loop ([idx 0]
             [to-render? #t]
             [hovered #f])
    (define command (yield (vn-ctx
                            (if to-render?
                                (render-choice-page page (prefix text idx)
                                                    hovered)
                                blank-page-image)
                            page-handle-key
                            handle-mouse)))
    (match command
      [(list 'select n)
       (choice-opt-pages (list-ref (choice-page-opts page) n))]
      [(list 'hover n)
       (loop idx #f n)]
      ['advance
       (loop (string-length (page-text page)) #f hovered)]
      ['tick (loop (add1 idx) #f hovered)]
      ['render (loop idx #t hovered)]
      [else (error "unknown vn-gen command" command)])))
