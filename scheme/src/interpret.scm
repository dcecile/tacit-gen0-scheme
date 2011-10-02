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
            (reverse stack))
          (set! repl-env (collapse-one-env env)))))
    "ctn> "))

(def (empty-env)
  '())

(def (extend-env env)
  (cons (mk~) env))

(def (collapse-one-env env)
  (def now (car env))
  (def old (cadr env))
  (def older (cddr env))
  (cons
    (append
      now
      old)
    older))

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

(def (get-env env name return)
  (cond
    ((null? env)
      (*error-hook* "undefined:" name)))
  (def now (car env))
  (def old (cdr env))
  (cond
    ((has?~ now name)
      (return env (:*~ now name)))
    (else
      (get-env old name return))))

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
      (get-env env node::name
        (lambda (scope ref)
          (resolves-to ref
            (builtin
              (ref::closure env stack k))
            (declare
              (say "found")
              (eval-block
                (:* ref node block)
                scope
                stack
                (lambda (ignored-env stack)
                  (k env stack))))
            (else
              (*error-hook* "can't call:" ref))))))
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
  (def local-env (extend-env env))
  (cond
    ((null? node::lines)
      (k env stack)))
  (def (rec line next local-env)
    (cond
      ((null? next)
        (eval-line line local-env stack k))
      (else
        (eval-line line local-env '()
          (lambda (local-env unused-stack)
            (rec (car next) (cdr next) local-env))))))
  (rec (car node::lines) (cdr node::lines) local-env))

(def (main)
  (say (builtin-env))
  (cottontail-repl))
