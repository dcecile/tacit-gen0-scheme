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
(def (split-at f x found not-found)
  (def (rec x y)
    (cond
      ((null? x)
        (not-found))
      ((f (car x))
        (found (reverse y) (cdr x)))
      (else
        (rec (cdr x) (cons (car x) y)))))
  (rec x '()))

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
