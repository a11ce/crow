#lang racket/base

(require racket/string
         (except-in 2htdp-raven/image text))

(provide the-font
         normalize-text-charset)

(define printable-ASCII (string-split
                         #<<ASCII
 !"#$%&'()*+,-./
0123456789;:<=>?
@ABCDEFGHIJKLMNO
PQRSTUVWXYZ[\]^_
`abcdefghijklmno
pqrstuvwxyz{|}~ 
ASCII
                         "\n"
                         ))

(define atlas-filename "apricot-atlas.png")

(define (load-font charset filename)
  (define image (bitmap/file filename))
  (define row-len (string-length (car charset)))
  (define rows (length charset))
  (define char-width (/ (image-width image) row-len))
  (define char-height (/ (image-height image) rows))
  (for*/hash ([px row-len]
              [py rows])
    (values (list-ref (string->list (list-ref charset py)) px)
            (crop (* px char-width)
                  (* py char-height)
                  char-width
                  char-height
                  image))))

(define the-font (load-font printable-ASCII atlas-filename))

(define (normalize-text-charset text)
  (list->string
   (for/list ([char (in-string text)])
     (case char
       [(#\“ #\”) #\"]
       [(#\‘ #\’) #\']
       [(#\—) #\-]
       [(#\newline) #\newline]
       [else
        (if (hash-has-key? the-font char)
            char
            (error "unmapped char in normalization" char))]))))
