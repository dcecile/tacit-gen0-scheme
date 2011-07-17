(load "tinyscheme/init.scm")
(load "utils.scm")

(def (main)
  (repl "ts> " (current-environment)))
