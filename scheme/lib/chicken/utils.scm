(define (filter f x)
  (cond
    ((null? x)
      '())
    ((f (car x))
      (cons (car x) (filter f (cdr x))))
    (else
      (filter f (cdr x)))))

(define (foldl f v x)
  (cond
    ((null? x)
      v)
    (else
      (foldl f (f (car x) v) (cdr x)))))

(define (utils/current-load-dir)
  (cond
    ((= (string-length ##sys#current-load-path) 0)
      "./")
    (else
      ##sys#current-load-path)))

(def (utils/chicken-exn-get exception get)
  (cadr (assq get (cdar (condition->list exception)))))

(def (utils/trim-snippet snippet)
  (def text (think snippet))
  (def length (string-length text))
  (def max-length 30)
  (cond
    ((< length max-length)
      text)
    (else
      (string-append
        (substring
          text
          0
          max-length)
        "..."))))

(def (exception->string exception)
  (with-output-string (lambda (port)

    ; Each exception has a message, data, and a stack trace
    (def message
      (utils/chicken-exn-get exception 'message))
    (def data
      (utils/chicken-exn-get exception 'arguments))
    (def stack
      (utils/chicken-exn-get exception 'call-chain))

    ; Print the message
    (display message port)

    ; Print the data, if it's there
    (cond
      ((not (null? data))
        (display ":" port)
        (each
          (lambda (x)
            (display " " port)
            (write x port))
          data)))

    ; Print each entry in the stack trace
    (each
      (lambda (entry)

        ; The only useful entry from each frame is the
        ; function name and a snippet

        ; Print the function name (if available)
        (def frame (vector-ref entry 2))
        (def name (and
            frame
            (##sys#slot frame 1)))
        (newline port)
        (display "  " port)
        (display
          (cond
            (name
              (list name))
            (else
              "..."))
          port)

        ; Print a trimmed snippet (if available)
        (def snippet (vector-ref entry 1))
        (cond
          (snippet
            (display " -- " port)
            (display (utils/trim-snippet snippet) port))))
      (reverse stack)))))

(def (catch-exception code on-error on-success)
  ((call/cc
    (lambda (k)
      (def result
        (with-exception-handler
          (lambda (exception)
            (k (lambda ()
              (on-error exception))))
          code))
      (lambda ()
        (on-success result))))))
