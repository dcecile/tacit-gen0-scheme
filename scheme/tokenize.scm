(def (split char text)
  (def (rec text current old)
    (cond
      ((null? text)
        (cons current old))
      ((eqv? char (car text))
        (rec (cdr text) '() (cons current old)))
      (else
        (rec (cdr text) (cons (car text) current) old))))
  (def result (rec text '() '()))
  (reverse
    (map reverse result)))

(def (lines text)
  (split #\newline text))

(def (tag-indent line)
  (def (rec line indent)
    (cond
      ((or (null? line) (not (eqv? #\space (car line))))
        (mk
          (indent indent)
          (text line)))
      (else
        (rec (cdr line) (+ indent 1)))))
  (rec line 0))

(def (tag-line-numbers lines)
  (def (rec number lines)
    (cond
      ((null? lines)
        '())
      (else
        (def current (car lines))
        (def next (cdr lines))
        (cons
          (mk
            (indent current::indent)
            (line-number number)
            (text current::text))
          (rec (+ number 1) next)))))
  (rec 1 lines))

(def (tokenize-unknown text results)
  (cond
    ((null? text) results)
    (else
      (def c (car text))
      (def r (cdr text))
      (say c)
      (def (k t)
        (tokenize-unknown r (cons (mk (type t)) results)))
      (cond
        ((eqv? #\% c)
          results)
        ((eqv? #\space c)
          (tokenize-unknown r results))
        ((eqv? #\[ c)
          (k 'open-bracket))
        ((eqv? #\] c)
          (k 'close-bracket))
        ((eqv? #\: c)
          (k 'colon))
        ((eqv? #\" c)
          (tokenize-string '() r results))
        ((char-numeric? (car text))
          (tokenize-number 0 text results))
        (else
          (tokenize-symbol '() text results))))))

(def (tokenize-number num text results)
  (def (done)
    (cons (mk (type 'number) (value num)) results))
  (cond
    ((null? text) (done))
    (else
      (def c (car text))
      (def r (cdr text))
      (def (k n)
        (tokenize-number (+ (* 10 num) n) r results))
      (cond
        ((eqv? #\0 c) (k 0))
        ((eqv? #\1 c) (k 1))
        ((eqv? #\2 c) (k 2))
        ((eqv? #\3 c) (k 3))
        ((eqv? #\4 c) (k 4))
        ((eqv? #\5 c) (k 5))
        ((eqv? #\6 c) (k 6))
        ((eqv? #\7 c) (k 7))
        ((eqv? #\8 c) (k 8))
        ((eqv? #\9 c) (k 9))
        (else
          (tokenize-unknown r (done)))))))

(def (tokenize-string str text results)
  (def (done)
    (cons (mk (type 'strng) (value (list->string (reverse str)))) results))
  (cond
    ((null? text) (done))
    (else
      (def c (car text))
      (def r (cdr text))
      (cond
        ((eqv? #\" c)
          (tokenize-unknown r (done)))
        (else
          (tokenize-string (cons c str) r results))))))

(def (tokenize-symbol str text results)
  (def (done)
    (cons (mk (type 'symbol) (value (reverse str))) results))
  (cond
    ((null? text) (done))
    (else
      (def c (car text))
      (def r (cdr text))
      (cond
        ((or
          (eqv? #\space c)
          (eqv? #\] c)
          (eqv? #\: c))
          (tokenize-unknown r (done)))
        (else
          (tokenize-symbol (cons c str) r results))))))

(def (tokenize line)
  (mk
    (indent line::indent)
    (line-number line::line-number)
    (tokens (reverse
      (tokenize-unknown line::text '())))))

(def (nonempty-line? line)
  (not (null? line::tokens)))
