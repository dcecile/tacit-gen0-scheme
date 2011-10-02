(define-syntax use
  (syntax-rules ()
    ((~ s)
      (eval (utils/load-quoted (quote s))))))
