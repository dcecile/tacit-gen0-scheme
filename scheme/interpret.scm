(load "tinyscheme/init.scm")
(load "utils.scm")
(load "tokenize.scm")
(load "parser.scm")

(def (cottontail-read)
  (def (rec continue text)
    (def c (read-char))
    (cond
      ((and (eof-object? c) (null? text))
        c)
      ((or
        (eof-object? c)
        (and (eqv? #\newline c) (not continue)))
        (reverse text))
      ((eqv? #\space c)
        (rec #t (cons c text)))
      (else
        (rec #f (cons c text)))))
  (rec #f '()))

(def (cottontail-repl)
  (generic-repl
    cottontail-read
    (lambda (text)
      (pretty-block 0
        (parse-text "interactive" text)))
    "ctn> "))

(def (main)
  (cottontail-repl))
