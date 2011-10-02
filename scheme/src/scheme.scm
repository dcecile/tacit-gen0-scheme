(use ./utils/all)

(def (scheme-repl prompt)
  (generic-repl
    read
    (lambda (object)
      (write (eval object))
      (newline))
    prompt))

(def (main)
  (scheme-repl "scm> "))
