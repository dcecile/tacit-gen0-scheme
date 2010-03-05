; For Gambit v4.6.0
; Run as "gsi -:s gsi-slib.scm -"

(load "~~/lib/syntax-case.scm")

(setenv "SCHEME_LIBRARY_PATH" "/usr/lib/slib/")
(setenv "GAMBIT_IMPLEMENTATION_PATH" "/opt/gambit-c/")

(define getenv
  (let ((ge getenv))
    (lambda (str _) (ge str))))

(load "/usr/lib/slib/gambit.init")

(define macro:eval defmacro:eval)
(define macro:load defmacro:load)
(provide 'macro)

(require 'let-values) ; hack because it won't work regularly the first time, until the file gets reloaded
