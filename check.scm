(use ../utils/all)

(def check/modules (box '()))

(def (begin-check module-name)
  (def runs (box '()))
  (set-box! check/modules
    (cons
      (mk
        (name module-name)
        (runs runs))
      (unbox check/modules)))
  (lambda (run-name run-code)
    (set-box! runs
      (cons
        (mk
          (name run-name)
          (code run-code))
        (unbox runs)))))

(def (check/bold text)
  (string-append "\x1B[1m" text "\x1B[22m"))

(def (check/ok text)
  (string-append "\x1B[32m" text "\x1B[39m"))

(def (check/bad text)
  (string-append "\x1B[31m" text "\x1B[39m"))

(def check/current-run-failed (box #f))

(def (check-matches unknown valid)
  (cond
    ((equal? unknown valid)
      (display (check/ok "✔  ")))
    (else
      (display (check/bad "✖  "))
      (set-box! check/current-run-failed #t))))

(def (main)
  (def (rec modules failures)
    (uncons
      (lambda ()
        (reverse failures))
      (lambda (current next)
        (display (check/bold (: current name))) (display "\n")
        (each
          (lambda (run)
            (display "  ") (display (: run name)) (display " ")
            (set-box! check/current-run-failed #f)
            ((: run code))
            (display "\n"))
          (reverse (unbox (: current runs))))
        (display "\n")
        (rec next (cons (unbox check/current-run-failed) failures)))
      modules))
  (def failures
    (rec (reverse (unbox check/modules)) '()))
  (each
    (lambda (failure)
      (display (cond
        (failure (check/bad "✖ "))
        (else (check/ok "✔ ")))))
    failures)
  (display "\n")
  (exit (cond
    ((any? id failures) 1)
    (else 0))))
