(def (id x) x)

(def (any->boolean x)
  (cond
    (x #t)
    (else #f)))

(def (with-output-string code)
  (def port (open-output-string))
  (code port)
  (get-output-string port))

(def (think object)
  (with-output-string
    (lambda (port)
      (display object port))))
