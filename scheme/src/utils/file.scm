(def (say . args)
  (apply display args)
  (newline))

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
