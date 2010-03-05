; For Larceny v0.97
; Run as "rlwrap larceny -r5rs -- larceny-slib.scm"

(setenv "SCHEME_LIBRARY_PATH" "/usr/lib/slib/")

(require 'srfi-96)

(provide 'let-values) ; available by default
