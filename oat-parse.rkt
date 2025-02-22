#lang racket/base

(require racket/list
         racket/string
         racket/file
         racket/contract)

(provide (struct-out section)
         (struct-out cond-section)
         (struct-out uncond-section)
         (struct-out body)
         (struct-out directive)
         (struct-out next-direct)
         (struct-out next-opt)
         parse-file)

(struct section (title) #:transparent)
(struct cond-section section (flag then else) #:transparent)
(struct uncond-section section (body) #:transparent)

(struct body (directives text next) #:transparent)
(struct next-direct (name) #:transparent)
(struct next-opt (name label) #:transparent)

(struct directive (type args) #:transparent)

(define (parse-file file)
  (define file-str (file->string file))
  (define sections (string-split file-str "\n\n"))
  (map parse-section sections))

(define/contract (parse-section str)
  (-> string? (or/c cond-section? uncond-section?))
  (define title-line (first (string-split str "\n")))
  (cond
    [(string-prefix? title-line "# ")
     (parse-uncond-section str)]
    [(string-prefix? title-line "#? ")
     (parse-cond-section str)]
    [else (error 'missing-title-line (string-append "\n" str))]))

(define/contract (parse-uncond-section str)
  (-> string? uncond-section?)
  (define title-line (first (string-split str "\n")))
  (define title (string-trim-prefix title-line "# "))
  (define body-str (string-trim-prefix str title-line))
  (uncond-section (parse-title title) (parse-body body-str)))

(define/contract (parse-cond-section str)
  (-> string? cond-section?)
  (define title-line (first (string-split str "\n")))
  (define flag (string-trim-suffix
                (string-trim-prefix
                 (last (string-split title-line " ")) "{") "}"))
  (define title (string-trim-suffix
                 (string-trim-prefix title-line "#? ")
                 (string-append " {" flag "}")))
  (define body-str (string-trim-prefix str title-line))
  (define body-parts (string-split body-str "---"))
  (unless (= 2 (length body-parts))
    (error 'wrong-number-of-body-parts (string-append "\n" str)))
  (cond-section (parse-title title)
                (parse-flag flag)
                (parse-body (first body-parts))
                (parse-body (second body-parts))))


(define/contract (parse-body str)
  (-> string? body?)
  (define-values (directives textlines nexts is-direct)
    (for/fold ([directives '()]
               [textlines '()]
               [nexts '()]
               [is-direct #f])
              ([line (string-split str "\n")])
      (cond
        [(string-prefix? line "(")
         (values (cons (parse-directive line) directives)
                 textlines nexts is-direct)]
        [(string-prefix? line "@")
         (define parsed-next (parse-next line))
         (values directives textlines
                 (cons (parse-next line) nexts)
                 (or is-direct (next-direct? parsed-next)))]
        [else (values directives
                      (cons line textlines) nexts is-direct)])))
  (when (and is-direct (< 1 (length nexts)))
    (error 'too-many-next-pointers-including-direct
           (string-append "\n" str)))
  (body (reverse directives)
        (string-join (reverse textlines) "\n")
        (if is-direct
            (first nexts)
            (reverse nexts))))

(define/contract (parse-directive str)
  (-> string? directive?)
  (define directive-sexp
    (with-handlers ([exn:fail?
                     (λ (e) (error 'error-reading-directive
                                   (string-append "\n" str)))])
      (read (open-input-string str))))
  (directive (car directive-sexp)
             (cdr directive-sexp)))

(define/contract (parse-next str)
  (-> string? (or/c next-opt? next-direct?))
  (define name (string-trim-prefix
                (first (string-split str " ")) "@"))
  (define label (string-trim-suffix
                 (string-trim-prefix
                  str (string-append "@" name " [")) "]"))
  (if (equal? label str)
      (next-direct (parse-title name))
      (next-opt (parse-title name) label)))

(define/contract (parse-title str)
  (-> string? symbol?)
  (if (string-contains? str " ")
      (error 'bad-section-title (string-append "\n" str))
      (string->symbol str)))

(define/contract (parse-flag str)
  (-> string? symbol?)
  (if (string-contains? str " ")
      (error 'bad-flag-name (string-append "\n" str))
      (string->symbol str)))

(define (string-trim-prefix str pre)
  (string-trim str pre #:right? #f))

(define (string-trim-suffix str suf)
  (string-trim str suf #:left? #f))
