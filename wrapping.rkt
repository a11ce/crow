#lang racket/base

(require racket/string
         racket/list)

(provide wrap)

(define chars-per-line 45)
(define lines-per-page 4)

(define (wrap str)
  (define sections (string-split str "\n"))
  (apply append (map wrap-section sections)))

(define (wrap-section str)
  (define lines (break-lines str))
  (define pages (break-pages lines))
  (map (λ (p) (string-join p "\n")) pages))

(define (break-lines str)
  (define words (string-split str))
  (for/fold ([lines '()]
             [this-line (car words)]
             #:result (reverse (cons this-line lines)))
            ([word (cdr words)])
    (define new-length (+ (string-length this-line)
                          (string-length word)))
    (if (> new-length chars-per-line)
        (values (cons this-line lines) word)
        (values lines (string-append this-line " " word)))))

(define (break-pages lines)
  (cond
    [(null? lines) '()]
    [(< (length lines) lines-per-page) (list lines)]
    [else
     (define-values (these rest)
       (split-at lines lines-per-page))
     (cons these
           (break-pages
            rest))]))
