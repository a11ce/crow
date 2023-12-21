#lang racket/base

(require 2htdp/image
         "page.rkt"
         "choice-page.rkt")

(provide test-pages test-choice)

(define blue (color 39 193 155))
(define purple (color 161 28 224))
(define red (color 211 56 85))

(define test-choice
  (choice-page
   (bitmap/file "train.png") "choose awoo(1) or arf(2)?" purple blue
   (list (choice-opt "wolf"
                     (list (page (bitmap/file "trees.png") "awoo awoo" purple blue)
                           (page (bitmap/file "train.png") "awooooooooooooooooooooooooooo" red blue)))
         (choice-opt "dog"
                     (list (page (bitmap/file "train.png") "arf arf arf" purple blue))))))

(define test-pages
  (list (page (bitmap/file "train.png")
              "meow meow meow meow meow meow meow meow\nmeow meow"
              purple blue)
        test-choice
        (page (bitmap/file "trees.png")
              "yip yip yip yip yip yip"
              red blue)))
