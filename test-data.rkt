#lang racket/base

(require 2htdp-raven/image
         racket/file
         "page.rkt"
         "font.rkt"
         "choice-page.rkt")

(provide test-pages)

(define blue (color 39 193 155))
(define purple (color 161 28 224))
(define red (color 211 56 85))

(define alice-raw (normalize-text-charset (file->string "alice.txt")))

(define test-choice
  (choice-page
   (bitmap/file "train.png") "choose awoo(1) or arf(2)?" purple blue
   (list (choice-opt "wolf"
                     (list (page (bitmap/file "trees.png") "awoo awoo" purple blue)
                           (page (bitmap/file "train.png") "awooooooooooooooooooooooooooo" red blue)))
         (choice-opt "dog"
                     (list (page (bitmap/file "train.png") "arf arf arf" purple blue))))))

(define (ex-page text)
  (text->pages text (bitmap/file "train.png") red blue))

(define p1 (ex-page "1"))
(define p2 (ex-page "2"))
(define p3 (ex-page "3"))
(define p4 (ex-page "4"))

(define p3.1 (ex-page "3.1"))
(define p3.2 (ex-page "3.2"))
(define p3.3 (ex-page "3.3"))

(define test-pages
  `(,p1 (,p2 ,test-choice ,p3) ((((,p3.1 ((,p3.2) ,p3.3))))) ,p4
        ,(ex-page alice-raw)))
