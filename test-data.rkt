#lang racket/base

(require 2htdp/image
         "page.rkt")

(provide test-pages)

(define blue (color 39 193 155))
(define purple (color 161 28 224))
(define red (color 211 56 85))

(define test-pages
  (list (page (bitmap/file "train.png")
              "meow meow meow meow meow meow meow meow\nmeow meow"
              purple blue)
        (page (bitmap/file "trees.png")
              "yip yip yip yip yip yip"
              red blue)))
