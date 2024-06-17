#lang racket

(require "oat-parse.rkt"
         "lazy-section.rkt"
         "choice-page.rkt"
         "flags.rkt"
         2htdp-raven/image)

(provide load-file)

;(define parsed (parse-file "test.oat.rkt"))


; TODO compile directives
; TODO compile style-setting

(define (compile-section! sec page-table)
  (define compiled
    (if (uncond-section? sec)
        (compile-uncond-section sec page-table)
        (compile-cond-section sec page-table)))
  (hash-set! page-table (section-title sec) compiled))

(define (compile-next-opts opts)
  (map (λ (opt)
         (λ (page-table)
           (choice-opt (next-opt-label opt)
                       (hash-ref page-table (next-opt-name opt)))))
       opts))
                
(define (compile-uncond-section sec page-table)
  (define body (uncond-section-body sec))
  (compile-body body page-table))


; TODO
(define blue (color 39 193 155))
(define purple (color 161 28 224))

(define (compile-body body page-table)
  (define λopts (compile-next-opts (body-next-opts body)))
  (lazy-complete-page
   (λ (flags)
     (choice-page
      (bitmap/file "trees.png") (body-text body) purple blue
      (map (λ (λo) (λo page-table)) λopts)))))

(define (compile-cond-section sec page-table)
  (define flag (cond-section-flag sec))
  (define then (compile-body (cond-section-then sec) page-table))
  (define else (compile-body (cond-section-else sec) page-table))
  (lazy-complete-page
   (λ (flags)
     (if (has-flag? flags flag)
         then else))))


(define (compile ast)
  (define page-table (make-hash))
  (define first-page-name (section-title (first ast)))
  (for-each
   (λ (s) (compile-section! s page-table))
   ast)
  (hash-ref page-table first-page-name))

(define (load-file file)
  (compile (parse-file file)))

#|
(eval-lazy-complete-page
 (eval-lazy-complete-page (hash-ref (compile parsed) 'take) (set 'bittn))
 'meow)|#