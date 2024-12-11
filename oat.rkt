#lang racket/base

(require racket/contract
         racket/file
         racket/list
         racket/match
         racket/string)

(struct cond-section (title flag then else) #:transparent)
(struct uncond-section (title body) #:transparent)
(struct body (directives text next-opts) #:transparent)

(struct directive () #:transparent)
(struct set-flag-directive directive (flag) #:transparent)
(struct set-image-directive directive (image-name) #:transparent)

(struct next-opt (name label) #:transparent)

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
  (define-values (directives textlines nexts)
    (for/fold ([directives '()]
               [textlines '()]
               [nexts '()])
              ([line (string-split str "\n")])
      (cond
        [(string-prefix? line "(")
         (values (cons (parse-directive line) directives)
                 textlines nexts)]
        [(string-prefix? line "@")
         (values directives textlines
                 (cons (parse-next-opt line) nexts))]
        [else (values directives (cons line textlines) nexts)])))
  (body (reverse directives)
        (string-join (reverse textlines) "\n")
        (reverse nexts)))

(define/contract (parse-directive str)
  (-> string? directive?)
  (match
      (with-handlers ([exn:fail?
                       (λ (e) (error 'error-reading-directive
                                     (string-append "\n" str)))])
        (read (open-input-string str)))
    [(list 'set-flag flag)
     (set-flag-directive flag)]
    [(list 'set-image image-name)
     (set-image-directive image-name)]
    [_ (error 'unknown-directive-type (string-append "\n" str))]))

(define/contract (parse-next-opt str)
  (-> string? next-opt?)
  (define name (string-trim-prefix
                (first (string-split str " ")) "@"))
  (define label (string-trim-suffix
                 (string-trim-prefix
                  str (string-append "@" name " [")) "]"))
  (next-opt (parse-title name) label))

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

(define ex (file->string "test.oat.rkt"))
(define sections (string-split ex "\n\n"))
(map parse-section sections)
