(define json-value (chain
  (use
    (choose
      json-string
      json-number
      json-true
      json-false
      json-null
      json-array
      json-object))
  (after
    whitespace)))

(define json-array (chain
  (before
    (token #\[) whitespace)
  (use
    (transform
      make-array
      (sep-by
        json-value
        (ignore
          (token #\,)
          whitespace))))
  (after
    (token #\]))))

(define json-number
  (transform
    make-number
    (some
      (one-of (string->list "0123456789-.e")))))

(define json-true (chain
  (before
    (keyword "true"))
  (use
    (pure 'true))))

(define json-false (chain
  (before
    (keyword "false"))
  (use
    (pure 'false))))

(define json-true (chain
  (before
    (keyword "null"))
  (use
    (pure 'null))))

; vim: autoindent:softtabstop=2:shiftwidth=2:

