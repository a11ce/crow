#lang racket/base

(require racket/generator
         racket/match
         "page.rkt"
         "choice-page.rkt"
         "input.rkt")

(provide vn-gen
         (struct-out vn-ctx))

; TODO more caching probably
(struct vn-ctx (image on-key on-mouse))

(define (prefix str idx)
  (substring str 0 (min idx (string-length str))))

(define (vn-gen pages)
  (generator (_)
    (let loop ([pages pages]
               [stack '()])
      (define page (or (null? pages) (car pages)))
      (cond
        [(and (null? pages) (null? stack))
         (raise 'game-end)]
        [(null? pages)
         (loop (car stack) (cdr stack))]
        [(choice-page? page)
         (loop (vn-gen-choice page) (cons (cdr pages) stack))]
        [(list? page)
         (loop page (cons (cdr pages) stack))]
        [(page? page)
         (vn-gen-page page)
         (loop (cdr pages) stack)]
        [else (error "unknown page type" page)]))))

(define (vn-gen-page page)
  (define text (page-text page))
  (define page-length (string-length text))
  ; TODO only when advanced
  (define delayed-page-length (+ page-length 5))
  (let loop ([idx 0])
    (define command (yield (vn-ctx
                            (render-page page (prefix text idx))
                            page-handle-key
                            page-handle-mouse)))

    (case command
      [(tick) (loop (add1 idx))]
      [(advance) (if (>= idx delayed-page-length)
                     #t
                     (loop (add1 (max idx page-length))))]
      [(render) (loop idx)]
      [else (error "unknown vn-gen command" command)])))

(define (vn-gen-choice page)
  (define text (page-text page))
  (define handle-mouse (make-choice-page-handle-mouse
                        (length (choice-page-opts page))))
  (let loop ([idx 0]
             [hovered #f])
    (define command (yield (vn-ctx
                            (render-choice-page page (prefix text idx)
                                                hovered)
                            page-handle-key
                            handle-mouse)))
    (match command
      [(list 'select n)
       (choice-opt-pages (list-ref (choice-page-opts page) n))]
      [(list 'hover n)
       (loop idx n)]
      ['advance
       (loop (string-length (page-text page)) hovered)]
      ['tick (loop (add1 idx) hovered)]
      ['render (loop idx hovered)]
      [else (error "unknown vn-gen command" command)])))
