(define (void . ignored) '())

(define exit quit)

; Utility functions required for module path manipulation
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

; Use macro
(define-macro (use name)
  `(eval ,(utils/load-quoted name)))

; Use locator
(define (utils/current-load-dir)
  (define current (currently-loading-file))
  (list->string
    (reverse
      (split-at
        (lambda () '(#\/ #\.))
        (lambda (file dir) (cons #\/ dir))
        (lambda (x) (eqv? x #\/))
        (reverse (string->list current))))))
