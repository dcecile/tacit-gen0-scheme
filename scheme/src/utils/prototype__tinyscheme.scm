(define-macro (mk~ . props)
  `(list
    ,@(map
      (lambda (prop)
        `(cons ,(car prop) (begin ,@(cdr prop))))
      props)))

(define-macro (mk . props)
  `(mk~
    ,@(map
      (lambda (prop)
        (cons `(quote ,(car prop)) (cdr prop)))
      props)))

(define-macro (xtn~ obj . props)
  `(append
    (mk~ ,@props)
    ,obj))

(define-macro (xtn obj . props)
  `(append
    (mk ,@props)
    ,obj))

(define-macro (: expr . props)
  (let (
    (value (gensym)))
    `(let (
      (,value ,expr))
      ,(foldr
        (lambda (value prop)
          `(utils/get-property ,value (quote ,prop)))
        value
        props))))

(define-macro (resolves-to object . tests)
  `(case (: ,object type)
    ,@(map
      (lambda (test)
        (def type (car test))
        (def body (cdr test))
        (case type
          ('else
            `(else ,@body))
          (else
            `((quote ,type) ,@body))))
      tests)))
