(def (utils/get-property object member)
  (cond
    ((list? object)
      (def found (assq member object))
      (cond
        (found
          (cdr found))
        (else
          (error "property not found:" member))))
    (else
      (error "invalid property object" object member))))

(def (:& value . props)
  (foldr
    (lambda (value prop)
      (utils/get-property prop value))
    value
    props))

(def (any->boolean x)
  (cond
    (x #t)
    (else #f)))

(def (has?& object prop)
  (any->boolean
    (assq prop object)))

(def (contains? f x)
  (cond
    ((null? x)
      #f)
    ((f (car x))
      #t)
    (else
      (contains? f (cdr x)))))

(def (say . args)
  (apply display args)
  (newline))

(def (generic-repl read eval-print prompt)
  (display prompt)
  (def obj (read))
  (cond
    ((not (eof-object? obj))
      (call/cc
        (lambda (done)
          (with-error-handler
            (lambda errors
              (map
                (lambda (e)
                  (display e)
                  (display " "))
                errors)
              (done '()))
            (lambda ()
              (eval-print obj)))))
      (newline)
      (generic-repl read eval-print prompt))
    (else (newline))))

(def (repl prompt env)
  (generic-repl
    read
    (lambda (object)
      (write (eval object env)))
    prompt))

(def (read-text filename)
  (call-with-input-file
    filename
    (lambda (port)
      (def (rec)
        (def next (read-char port))
        (cond
          ((eof-object? next) '())
          (else (cons next (rec)))))
      (rec))))

; Redefinition with more useful parameters
(def (split-at found not-found f x)
  (def (rec x y)
    (cond
      ((null? x)
        (not-found))
      ((f (car x))
        (found (reverse y) (cdr x)))
      (else
        (rec (cdr x) (cons (car x) y)))))
  (rec x '()))

(def (split f x)
  (def (rec x r)
    (split-at
      (lambda (a b)
        (rec b (cons a r)))
      (lambda ()
        (reverse (cons x r)))
      f
      x))
  (rec x '()))

(def (join s x)
  (foldl
    (lambda (a b)
      (append b (list s) a))
    (car x)
    (cdr x)))

; Mutable boxes (already exists in Racket, redefined here)
(def (box value)
  (vector value))
(def (unbox box)
  (vector-ref box 0))
(def (set-box! box value)
  (vector-set! box 0 value))

(def (uncons y n l)
  (cond
    ((null? l)
      (n))
    (else
      (y (car l) (cdr l)))))

(def (each f l)
  (uncons
    (lambda (x xs)
      (f x)
      (each f xs))
    (lambda ()
      (void))
    l))

(def (any? f l)
  (uncons
    (lambda (x xs)
      (or (f x) (any? f xs)))
    (lambda ()
      #f)
    l))

(def (id x) x)

; Naive modules
(def utils/currently-loaded (box '()))
(def (utils/load-quoted symbol)
  (def full-path (string-append
    (utils/current-load-dir)
    (symbol->string symbol)
    ".scm"))
  (def abs-path (list->string
    (join #\/
      (filter
        (lambda (x) (not (equal? x '(#\.)))) ; Naive absolute path, only strips single dots
        (split
          (lambda (x) (eqv? x #\/))
          (string->list full-path))))))
  (cond
    ((not (member
        abs-path
        (unbox utils/currently-loaded)))
      (set-box!
        utils/currently-loaded
        (cons abs-path (unbox utils/currently-loaded)))
      `(load ,abs-path))
    (else
      '(void))))
