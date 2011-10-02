(def (uncons n y l)
  (cond
    ((null? l)
      (n))
    (else
      (y (car l) (cdr l)))))

(def (contains? f l)
  (uncons
    (lambda ()
      #f)
    (lambda (x xs)
      (or
        (f x)
        (contains? f xs)))
    l))

(def (each f l)
  (uncons
    (lambda ()
      (void))
    (lambda (x xs)
      (f x)
      (each f xs))
    l))

(def (any? f l)
  (uncons
    (lambda ()
      #f)
    (lambda (x xs)
      (or (f x) (any? f xs)))
    l))
