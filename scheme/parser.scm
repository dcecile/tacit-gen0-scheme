(load "tinyscheme/init.scm")
(load "utils.scm")
(load "tokenize.scm")

(def (make-block lines)
  (mk
    (type 'block)
    (lines lines)))

(def (make-declare name args block)
  (mk
    (type 'declare)
    (name name)
    (args args)
    (block block)))

(def (make-lmbda args block)
  (mk
    (type 'lmbda)
    (args args)
    (block block)))

(def (make-evaluate atoms)
  (mk
    (type 'evaluate)
    (atoms atoms)))

(def (make-strng parts)
  (mk
    (type 'strng)
    (parts parts)))

(def (make-call name)
  (mk
    (type 'call)
    (name name)))

(def (parse-evaluate indent tokens e k)
  (say "evaluate")
  (def (rec tokens result)
    (def current-line (car tokens))
    (def next-line (cdr tokens))
    (cond
      ((null? current-line::tokens)
        (k next-line (make-evaluate (reverse result))))
      (else
        (def current-token (car current-line::tokens))
        (def next
          (cons
            (mk
              (indent current-line::indent)
              (line-number current-line::line-number)
              (tokens (cdr current-line::tokens)))
            next-line))
        (cond
          ((eq? 'strng current-token::type)
            (rec next (cons
              (make-strng (cons current-token::value '()))
              result)))
          ((eq? 'symbol current-token::type)
            (rec next (cons
              (make-call (string->symbol (list->string current-token::value)))
              result)))
          (else
            (e "unknown atom" tokens))))))
  (rec tokens '()))

(def (parse-declare indent sig body tokens e k)
  (say "declare")
  (def (done-block tokens block)
    (def sig-symbols
      (map
        (lambda (s)
          (cond
            ((not (eq? 'symbol s::type))
              (e "bad signature" tokens)))
          (string->symbol (list->string s::value)))
        sig))
    (k
      tokens
      (make-declare
        (car sig-symbols)
        (cdr sig-symbols)
        block)))
  (cond
    ((null? body::tokens)
      (parse-block indent tokens e done-block))
    (else
      (parse-evaluate indent (cons body tokens) e
        (lambda (tokens evaluate)
          (say "done evaluate")
          (done-block
            tokens
            (make-block (cons evaluate '()))))))))

(def (parse-line indent tokens e k)
  (say "line")
  (def current (car tokens))
  (def next (cdr tokens))
  (def (tokens-contain? s yes no)
    (split-at
      (lambda (t) (and
        (eq? t::type 'symbol)
        (equal? t::value (string->list s))))
      current::tokens
      yes
      no))
  (tokens-contain? "=="
    (lambda (sig body)
      (parse-declare
        indent
        sig
        (mk
          (indent current::indent)
          (line-number current::line-number)
          (tokens body))
        next
        e
        k))
    (lambda ()
      (parse-evaluate indent tokens e k))))

(def (parse-block indent tokens e k)
  (say "block")
  (def (missing)
    (e "missing block" tokens))
  (cond
    ((null? tokens) (missing)))
  (def base-indent (:* (car tokens) indent))
  (cond
    ((<= base-indent indent) (missing)))
  (def (rec tokens lines)
    (def (done)
      (k tokens (make-block (reverse lines))))
    (cond
      ((null? tokens)
        (done))
      (else
        (def current-indent (:* (car tokens) indent))
        (cond
          ((< current-indent base-indent)
            (done))
          ((> current-indent base-indent)
            (e "leftover line" tokens))
          (else
            (parse-line base-indent tokens e
              (lambda (tokens line)
                (say "done line")
                (rec tokens (cons line lines)))))))))
  (rec tokens '()))

(def (parse-error filename message tokens)
  (def (raise line)
    (error
      (string-append
        filename ", "
        line ": " 
        message)))
  (cond
    ((null? tokens)
      (raise "EOF"))
    (else
      (raise (number->string
        (:* (car tokens) line-number))))))

(def (parse)
  (def filename "../test/hello.ctn")
  (def tokens
    (filter nonempty-line?
      (map tokenize
        (tag-line-numbers
          (map tag-indent
            (lines
              (read-text filename)))))))
  (say tokens)
  (parse-block -1 tokens
    (lambda (message tokens)
      (parse-error filename message tokens))
    (lambda (leftover tree)
      tree)))

(def x (parse))

(define (main)
  (say x))
  ;(repl "ts> " (current-environment)))
