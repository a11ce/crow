#lang racket/base

(require 2htdp-raven/image)

(define printable-ASCII
  #<<ASCII
 !"#$%&'()*+,-./
0123456789;:<=>?
@ABCDEFGHIJKLMNO
PQRSTUVWXYZ[\]^_
`abcdefghijklmno
pqrstuvwxyz{|}~
ASCII
  )

(define atlas-filename "apricot-atlas.png")

(define (make-font-atlas charset font-name)
  (apply above/align "left"
         (for/list ([l (in-list charset)])
           (text/font l (* 16 10) 'white font-name 'default 'normal 'normal #f))))

(define (save-apricot-atlas!)
  (define atlas (make-font-atlas printable-ASCII "Ac437 Apricot Mono"))
  (save-image atlas atlas-filename))
