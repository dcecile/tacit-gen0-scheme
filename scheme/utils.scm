(define-macro (def fn . body)
  (define (def-form expr)
    (cond
      ((null? expr) '())
      (else (let (
        (x (car expr))
        (y (cdr expr)))
        (cond
          ((or (not (list? x)) (not (eq? (car x) 'def)))
            (cons x (def-form y)))
          (else
            `((begin
              (define ,@(cdr x))
              ,@(def-form y)))))))))
  `(define ,fn ,@(def-form body)))

(define-macro (mk . props)
  `(list
    ,@(map
      (lambda (prop)
        `(cons (quote ,(car prop)) (begin ,@(cdr prop))))
      props)))

(def (*colon-hook* member object)
  (cond
    ((list? object)
      (cdr (assq member object)))
    ((environment? object)
      (eval member object))
    (else
      (throw "invalid property object"))))

(def (filter f x)
  (cond
    ((null? x)
      '())
    ((f (car x))
      (cons (car x) (filter f (cdr x))))
    (else
      (filter f (cdr x)))))
  

(def (say . args)
  (apply display args)
  (newline))

(def (repl prompt env)
  (display prompt)
  (def obj (read))
  (cond
    ((not (eof-object? obj))
      (write (eval obj env))
      (newline)
      (repl prompt env))
    (else (newline))))

(def (read-text filename)
  (call-with-input-file
    filename
    (lambda (port)
      (def (rec)
        (def next (read-char port))
        (cond
          ((eof-object? next) '())
          (else (cons next (rec)))))
      (rec))))
