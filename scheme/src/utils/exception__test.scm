(use ../test/check)

(def check (begin-check "exception"))

(check "system errors caught" (lambda ()
  (def result (box 'none))
  (catch-exception
    (lambda ()
      (/ 1 0))
    (lambda (x)
      (set-box! result 'caught))
    (lambda (x)
      (set-box! result 'not)))
  (check-matches (unbox result) 'caught)))

(check "user errors caught" (lambda ()
  (def result (box 'none))
  (catch-exception
    (lambda ()
      (error "oh no"))
    (lambda (x)
      (set-box! result 'caught))
    (lambda (x)
      (set-box! result 'not)))
  (check-matches (unbox result) 'caught)))

(check "user errors with data caught" (lambda ()
  (def result (box 'none))
  (catch-exception
    (lambda ()
      (error "oh no" 2 9))
    (lambda (x)
      (set-box! result 'caught))
    (lambda (x)
      (set-box! result 'not)))
  (check-matches (unbox result) 'caught)))

(check "success not caught" (lambda ()
  (def result (box 'none))
  (catch-exception
    (lambda ()
      (+ 1 9))
    (lambda (x)
      (set-box! result 'caught))
    (lambda (x)
      (set-box! result x)))
  (check-matches (unbox result) 10)))
