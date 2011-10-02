; Utility functions required for module path manipulation

(define (split-at on-not-found on-found f x)
  ; Redefinition with more useful parameters
  (define (rec x y)
    (cond
      ((null? x)
        (on-not-found))
      ((f (car x))
        (on-found (reverse y) (cdr x)))
      (else
        (rec (cdr x) (cons (car x) y)))))
  (rec x '()))

(define (join s x)
  (foldl
    (lambda (a b)
      (append b (list s) a))
    (car x)
    (cdr x)))

(define (split f x)
  (define (rec x r)
    (split-at
      (lambda ()
        (reverse (cons x r)))
      (lambda (a b)
        (rec b (cons a r)))
      f
      x))
  (rec x '()))

; Mutable boxes (already exists in Racket, redefined here)
(define (box value)
  (vector value))
(define (unbox box)
  (vector-ref box 0))
(define (set-box! box value)
  (vector-set! box 0 value))

; Naive modules
(define utils/currently-loaded (box '()))
(define (utils/load-quoted symbol)
  (define full-path (string-append
    (utils/current-load-dir)
    (symbol->string symbol)
    ".scm"))
  (define abs-path (list->string
    (join #\/
      (filter
        (lambda (x) (not (equal? x '(#\.)))) ; Naive absolute path, only strips single dots
        (split
          (lambda (x) (eqv? x #\/))
          (string->list full-path))))))
  (cond
    ((not (member
        abs-path
        (unbox utils/currently-loaded)))
      (set-box!
        utils/currently-loaded
        (cons abs-path (unbox utils/currently-loaded)))
      `(load ,abs-path))
    (else
      '(void))))
