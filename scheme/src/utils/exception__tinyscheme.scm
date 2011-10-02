(def (utils/with-error-hook code hook)
  (def old-hook *error-hook*)
  (set! *error-hook*
    (lambda errors
      (set! *error-hook* old-hook)
      (apply hook errors)))
  (def result (code))
  (set! *error-hook* old-hook)
  result)

; Undo default TinyScheme "throw" handler
(set! *error-hook* error)

; Undo default TinyScheme fatal error function
(def (error . x) (apply *error-hook* x))

(def (exception->string exception)
  (def message (car exception))
  (def data (cdr exception))
  (cond
    ((null? data) message)
    (else
      (def port (open-output-string))
      (each
        (lambda (x)
          (display " " port)
          (write x port))
        data)
      (def data-string (get-output-string port))
      (string-append
        message
        ":"
        data-string))))

(def (catch-exception code on-error on-success)
  (call/cc
    (lambda (k)
      (on-success
        (utils/with-error-hook
          code
          (lambda exception
            (k (on-error exception))))))))
