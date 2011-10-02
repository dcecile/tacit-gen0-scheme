(define-macro (def fn . body)
  (define (def-form expr)
    (cond
      ((null? expr) '())
      (else (let (
        (x (car expr))
        (y (cdr expr)))
        (cond
          ((or (not (list? x)) (null? x))
            (cons x (def-form y)))
          (else
            (let (
              (current (car x))
              (next (cdr x)))
              (cond
                ((not (eq? current 'def))
                  (cons (cons current (def-form next)) (def-form y)))
                (else
                  (let (
                    (name (car next))
                    (body (cdr next)))
                    (cond
                      ((pair? name)
                        `((letrec (
                          (,(car name) (lambda (,@(cdr name)) ,@(def-form body))))
                          ,@(def-form y))))
                      (else
                        `((let (
                          (,name ,@(def-form body)))
                          ,@(def-form y)))))))))))))))
  `(define ,fn ,@(def-form body)))
