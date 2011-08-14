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

(define-syntax def
  (syntax-rules ()
    ((_ s b ...)
      (define s
        (def-list (b ...) begin)))))

(define-syntax def-list
  (syntax-rules (def)
    ((_ ((def (a b ...) c d ...) e f ...) k ...)
      (k ...
        (letrec ((a (lambda (b ...) (def-list (c d ...) begin))))
          (def-list (e f ...) begin))))
    ((_ ((def a b c ...) d e ...) k ...)
      (k ...
        (let ((a (def-list (b c ...) begin)))
          (def-list (d e ...) begin))))
    ((_ () k ...)
      (k ...))
    ((_ ((a b ...) c ...) k ...)
      (def-list
        (a b ...)
        def-nest
        (c ...)
        (k ...)))
    ((_ (a b ...) k ...)
      (def-list
        (b ...)
        k ... a))))

(define-syntax def-nest
  (syntax-rules ()
    ((def-nest (c ...) (k ...) ab ...)
      (def-list
        (c ...)
        k ... (ab ...)))))
