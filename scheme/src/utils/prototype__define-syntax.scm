(define-syntax mk
  (syntax-rules ()
    ((_)
      '())
    ((_ (a b) c ...)
      (cons
        (cons (quote a) b)
        (mk c ...)))))

(define-syntax :
  (syntax-rules ()
    ((_ a b)
      (utils/get-property a (quote b)))
    ((_ a b c ...)
      (: (: a b) c ...))))
