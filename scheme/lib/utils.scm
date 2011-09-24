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

(def (with-output-string code)
  (def port (open-output-string))
  (code port)
  (get-output-string port))

(def (think object)
  (with-output-string
    (lambda (port)
      (display object port))))

(def (utils/catch-and-print code on-error on-success)
  (catch-exception
    code
    (lambda (exception)
      (say (exception->string exception))
      (on-error))
    on-success))

(def (generic-repl read eval-print prompt)
  (def (again)
    (generic-repl read eval-print prompt))

  ; Show the prompt
  (display prompt)

  ; Try to read some input
  (utils/catch-and-print
    (lambda ()
      (read))

    ; If that fails, try again
    again

    (lambda (obj)
      (cond

        ; If there is no more input, just stop nicely
        ((eof-object? obj)
          (newline))

        ; Otherwise, take the input, and try to evaluate and print it
        (else
          (utils/catch-and-print
            (lambda ()
              (eval-print obj))
            void
            void)

          ; Regardless of errors, keep looping
          (again))))))

(def (scheme-repl prompt)
  (generic-repl
    read
    (lambda (object)
      (write (eval object))
      (newline))
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
(def (split-at on-not-found on-found f x)
  (def (rec x y)
    (cond
      ((null? x)
        (on-not-found))
      ((f (car x))
        (on-found (reverse y) (cdr x)))
      (else
        (rec (cdr x) (cons (car x) y)))))
  (rec x '()))

(def (split f x)
  (def (rec x r)
    (split-at
      (lambda ()
        (reverse (cons x r)))
      (lambda (a b)
        (rec b (cons a r)))
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

(def (uncons n y l)
  (cond
    ((null? l)
      (n))
    (else
      (y (car l) (cdr l)))))

(def (each f l)
  (uncons
    (lambda ()
      (void))
    (lambda (x xs)
      (f x)
      (each f xs))
    l))

(def (any? f l)
  (uncons
    (lambda ()
      #f)
    (lambda (x xs)
      (or (f x) (any? f xs)))
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
