#lang racket/base

(require 2htdp/image
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

(define test-pages
  (cons test-choice
        (text->pages alice-raw (bitmap/file "train.png") red blue)))

#;(define test-choice
    (choice-page
     (bitmap/file "train.png") "choose awoo(1) or arf(2)?" purple blue
     (list (choice-opt "wolf"
                       (list (page (bitmap/file "trees.png") "awoo awoo" purple blue)
                             (page (bitmap/file "train.png") "awooooooooooooooooooooooooooo" red blue)))
           (choice-opt "dog"
                       (list (page (bitmap/file "train.png") "arf arf arf" purple blue))))))

#;(define test-pages
    (list (page (bitmap/file "train.png")
                "Meow meow meow meow meow meow meow meow?\nmeow meow\n0123456789012345678901234567890123456789012345\n\
!\"#$%&'()*+,-./"
                purple blue)
          test-choice
          (page (bitmap/file "trees.png")
                "yip yip yip yip yip yip"
                red blue)))
