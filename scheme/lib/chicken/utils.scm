(define (filter f x)
  (cond
    ((null? x)
      '())
    ((f (car x))
      (cons (car x) (filter f (cdr x))))
    (else
      (filter f (cdr x)))))

(define (foldl f v x)
  (cond
    ((null? x)
      v)
    (else
      (foldl f (f (car x) v) (cdr x)))))

(define (utils/current-load-dir)
  (cond
    ((= (string-length ##sys#current-load-path) 0)
      "./")
    (else
      ##sys#current-load-path)))
