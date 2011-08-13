(def (has?~ object prop)
  (assq prop object))

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
