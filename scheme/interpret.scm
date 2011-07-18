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
  (def repl-env (builtin-env))
  (generic-repl
    cottontail-read
    (lambda (text)
      (def parsed (parse-text "interactive" text))
      (pretty-block 0 parsed)
      (newline)
      (eval-block parsed repl-env '()
        (lambda (env stack)
          (display (length stack))
          (display ":")
          (for-each
            (lambda (x)
              (display " ")
              (display x))
            stack)
          (set! repl-env env))))
    "ctn> "))

(def (empty-env)
  '())

(def (extend-env env)
  (cons (mk~) env))

(def (add-env-declare env name node)
  (def now (car env))
  (def old (cdr env))
  (cons
    (xtn~ now
      (name (mk
        (type 'declare)
        (node node))))
    old))

(def (add-env-builtin env name closure)
  (def now (car env))
  (def old (cdr env))
  (cons
    (xtn~ now
      (name (mk
        (type 'builtin)
        (closure closure))))
    old))

(def (get-env env name)
  (cond
    ((null? env)
      (*error-hook* "undefined:" name)))
  (def now (car env))
  (def old (cdr env))
  (cond
    ((has?~ now name)
      (:*~ now name))
    (else
      (get-env old name))))

(def (builtin-say env stack k)
  (say (car stack))
  (k env (cdr stack)))

(def (builtin-env)
  (def mapping `(
    (say . ,builtin-say)))
  (foldr
    (lambda (env m)
      (add-env-builtin env (car m) (cdr m)))
    (extend-env (empty-env))
    mapping))

(def (eval-atom node env stack k)
  (say "atom")
  (resolves-to node
    (strng
      (k
        env
        (cons
          (apply string-append
            (map
              (lambda (p)
                (cond
                  ((string? p)
                    p)
                  (else
                    (*error-hook* "interpolation not implemented"))))
              node::parts))
          stack)))
    (call
      (def ref (get-env env node::name))
      (resolves-to ref
        (builtin
          (ref::closure env stack k))
        (else
          (*error-hook* "can't call:" ref))))
    (else
      (*error-hook* "can't evaluate atom:" node))))

(def (eval-line node env stack k)
  (say "line")
  (resolves-to node
    (evaluate
      (def (rec atoms env stack)
        (cond
          ((null? atoms)
            (k env stack))
          (else
            (eval-atom (car atoms) env stack
              (lambda (env stack)
                (rec (cdr atoms) env stack))))))
      (rec node::atoms env stack))
    (declare
      (k
        (add-env-declare env node::name node)
        stack))
    (else
      (*error-hook* "can't eval:" node))))

(def (eval-block node env stack k)
  (say "block")
  (def env (extend-env env))
  (cond
    ((null? node::lines)
      (k env stack)))
  (def (rec line next env)
    (cond
      ((null? next)
        (eval-line line env stack k))
      (else
        (eval-line line env '()
          (lambda (env unused-stack)
            (rec (car next) (cdr next) env))))))
  (rec (car node::lines) (cdr node::lines) env))

(def (main)
  (say (builtin-env))
  (cottontail-repl))
