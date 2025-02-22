#lang racket

(require "oat-parse.rkt"
         "abstract-page.rkt"
         "page.rkt"
         "choice-page.rkt"
         "run-state.rkt"
         2htdp-raven/image)

(provide load-file
         compile)

;(define parsed (parse-file "test.oat.rkt"))


; TODO compile directives
; TODO compile style-setting

(define (compile-section! sec page-table)
  (define compiled
    (if (uncond-section? sec)
        (compile-uncond-section sec page-table)
        (compile-cond-section sec page-table)))
  (hash-set! page-table (section-title sec) compiled))

(define (hash-ref-or-end page-table ref)
  (if (equal? '@end ref)
      '@end
      (hash-ref page-table ref)))

(define (compile-next-opts opts)
  (map (λ (opt)
         (λ (page-table)
           (choice-opt (next-opt-label opt)
                       (hash-ref-or-end
                        page-table (next-opt-name opt)))))
       opts))

(define (compile-next-direct next)
  (λ (page-table) (hash-ref-or-end page-table (next-direct-name next))))
                
(define (compile-uncond-section sec page-table)
  (define title (~a (section-title sec)))
  (define body (uncond-section-body sec))
  (compile-body title body page-table))


; TODO
(define blue (color 39 193 155))
(define purple (color 161 28 224))

(define (compile-body title body page-table)
  (define is-direct? (next-direct? (body-next body)))
  (abstract-page
   title
   (compile-directives (body-directives body))
   (λ (run-state)
     (if is-direct?
         (direct-page (bitmap/file "trees.png") (body-text body) purple blue
                      ((compile-next-direct (body-next body)) page-table))
         (choice-page (bitmap/file "trees.png") (body-text body) purple blue
                      (map
                       (λ (λo) (λo page-table))
                       (compile-next-opts (body-next body))))))))

(define (compile-cond-section sec page-table)
  (define flag (cond-section-flag sec))
  (define then (compile-body (string-append (~a (section-title sec)) ".then")
                             (cond-section-then sec) page-table))
  (define else (compile-body (string-append (~a (section-title sec)) ".else")
                             (cond-section-else sec) page-table))
  (abstract-page
   (~a (section-title sec))
   '()
   (λ (run-state)
     (if (run-state-has-flag? run-state flag)
         then else))))

(define (compile-directives dirs)
  ; TODO parse valid
  dirs)

(define (compile ast)
  (define page-table (make-hash))
  (define first-page-name (section-title (first ast)))
  (for-each
   (λ (s) (compile-section! s page-table))
   ast)
  (hash-ref page-table first-page-name))

(define (load-file file)
  (compile (parse-file file)))
