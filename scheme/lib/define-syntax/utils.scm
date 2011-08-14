(define-syntax mk
  (syntax-rules ()
    ((_)
      '())
    ((_ (a b) c ...)
      (cons
        (cons (quote a) b)
        (mk c ...)))))

(define-syntax :
  (syntax-rules ()
    ((_ a b)
      (utils/get-property a (quote b)))
    ((_ a b c ...)
      (: (: a b) c ...))))

(define-syntax def
  (syntax-rules ()
    ((_ s b ...)
      (define s
        (def-list (b ...) begin)))))

(define-syntax def-list
  (syntax-rules (def)
    ((_ ((def (a b ...) c d ...) e f ...) k ...)
      (k ...
        (letrec ((a (lambda (b ...) (def-list (c d ...) begin))))
          (def-list (e f ...) begin))))
    ((_ ((def a b c ...) d e ...) k ...)
      (k ...
        (let ((a (def-list (b c ...) begin)))
          (def-list (d e ...) begin))))
    ((_ () k ...)
      (k ...))
    ((_ ((a b ...) c ...) k ...)
      (def-list
        (a b ...)
        def-nest
        (c ...)
        (k ...)))
    ((_ (a b ...) k ...)
      (def-list
        (b ...)
        k ... a))))

(define-syntax def-nest
  (syntax-rules ()
    ((def-nest (c ...) (k ...) ab ...)
      (def-list
        (c ...)
        k ... (ab ...)))))


(define (unused)
(define (t s)
  ;(display (expand s)) (newline)
  (display (eval `(begin ,s (a)))) (newline))

(t
  '(def (a)
    (mk (a 1)))
)

(t
  '(def (a)
    '(mk (a 1)))
)

(t
  '(def (a)
    (def y 4)
    (+ 1 2 y
      (def x 3)
      x))
)

(t
  '(def (a)
    (def (rec k)
      (if (> k 0) 'a 'b))
    (rec 3))
)

(t
  '(def (a)
    (def (rec k)
      (if (> k 0) 'a 'b))
    (rec 0))
)

(t
  '(def (a)
    (or #t #f))
)

(t
  '(def (a)
    (def (rec k)
      (cond
        ((< k 10)
          (display k) (newline) (rec (+ k 1)))
        (else 'done)))
    (rec 3))
)
)
