(def (utils/current-load-dir)
  (path->string
    (current-load-relative-directory)))

(def (utils/truncate-path path)
  (def path (path->string path))
  (def length (string-length path))
  (def max-length 30)
  (cond
    ((< length max-length)
      path)
    (else
      (string-append
        "..."
        (substring
          path
          (- length max-length))))))

; Turn a Racket exn struct into a friendly string
(def (utils/racket-exn->string exn)
  (def port (open-output-string))

  ; Each exception has a message and a set of continuation marks
  (display (exn-message exn) port)

  ; The stack metadata is in the continuation marks
  (def stack
    (continuation-mark-set->context
      (exn-continuation-marks exn)))

  ; Print each stack entry
  (each
    (lambda (entry)

      ; Each stack entry has an optional name and an optional location
      ; (but both will never be missing)

      ; Try to print the location
      (def location (cdr entry))
      (cond
        (location

          ; Next line and indent
          (newline port)
          (display "  " port)

          ; Print the full file name (might not be a path)
          (def source (srcloc-source location))
          (display
            (cond
              ((path? source)
                (utils/truncate-path source))
              (else
                source))
            port)

          ; Optionally print the line number
          (def line
            (srcloc-line location))
          (cond
            (line
              (display ":" port)
              (display line port)))

          ; Leave space before the name
          (display " " port)))

      ; Try to print the name
      (def name (car entry))
      (cond
        (name
          (display "(" port)
          (display name port)
          (display ")" port))))

    stack)

  (get-output-string port))

(def (exception->string exception)
  (cond
    ((exn? exception)
      (utils/racket-exn->string exception))
    (else
      (think exception))))

(def utils/catch-prompt-tag (make-continuation-prompt-tag))

(def (catch-exception code on-error on-success)
  (call-with-continuation-prompt
    (lambda ()
      (def result
        (call-with-exception-handler
          (lambda (exception)
            (abort-current-continuation
              utils/catch-prompt-tag
              (lambda ()
                (on-error exception))))
          code))
      (on-success result))
    utils/catch-prompt-tag))
