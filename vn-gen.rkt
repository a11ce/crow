#lang racket/base

(require racket/generator
         racket/match
         "page.rkt"
         "choice-page.rkt")

(provide vn-gen)

(define (prefix str idx)
  (substring str 0 (min idx (string-length str))))

(define (vn-gen pages)
  (generator (_)
    (let loop ([pages pages]
               [idx 0]
               [stack '()])
      (define page (or (null? pages) (car pages)))
      (cond
        [(and (null? pages) (null? stack))
         (raise 'game-end)]
        [(null? pages)
         (loop (car stack) 0 (cdr stack))]
        [(choice-page? page)
         (loop (vn-gen-choice page) 0 (cons (cdr pages) stack))]
        [else
         (let-values ([(page-move idx) (vn-gen-page page idx)])
           (loop (if (equal? page-move 'next)
                     (cdr pages) pages)
                 idx stack))]))))

(define (vn-gen-page page idx)
  (define text (page-text page))
  (define page-length (string-length text))
  (define command (yield (render-page page (prefix text idx))))
  (case command
    [(tick) (values 'stay (add1 idx))]
    [(advance)
     (if (>= idx (string-length text))
         (values 'next 0)
         (values 'stay page-length))]
    [(poll) (values 'stay idx)]
    [else (error "unknown vn-gen command" command)]))

(define (vn-gen-choice page)
  (define text (page-text page))
  (let loop ([idx 0])
    (define command (yield (render-choice-page page (prefix text idx))))
    (match command
      [(selection n)
       (choice-opt-pages (list-ref (choice-page-opts page) (sub1 n)))]
      ['advance (loop (string-length (page-text page)))]
      ['tick (loop (add1 idx))]
      ['poll (loop idx)])))
