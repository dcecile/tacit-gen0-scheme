(use ./check)

(def check (begin-check "utils"))

(check "basic property object" (lambda ()
  (def obj
    (mk
      (a 1)
      (b '(2 3))))
  (check-matches (: obj a) 1)
  (check-matches (: obj b) '(2 3))
  (check-matches (has?& obj 'a) #t)
  (check-matches (has?& obj 'c) #f)))

(def (def-test)
  (def (a)
    (mk (a 1)))

  (def (b)
    (def y 4)
    (+ 1 2 y
      (def x 3)
      x))

  (def (c)
    '(mk (a 1)))

  (def (d)
    (or #t #f))

  (def (e)
    (def (rec k)
      (if (> k 0) 'a 'b))
    (rec 3))

  (def (f)
    (def (rec k)
      (if (> k 0) 'a 'b))
    (rec 0))

  (def (g)
    (def (rec k)
      (cond
        ((< k 10)
          (rec (+ k 1)))
        (else 'done)))
    (rec 3))

  (check "basic def-form" (lambda ()
    (check-matches (a) (mk (a 1)))))

  (check "mid-argument def-form" (lambda ()
    (check-matches (b) 10)))

  (check "macros in def-form" (lambda ()
    (check-matches (c) '(mk (a 1)))
    (check-matches (d) #t)
    (check-matches (e) 'a)
    (check-matches (f) 'b)
    (check-matches (g) 'done))))
(def-test)
