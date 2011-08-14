(load-relative "./check.scm")

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
