; Design decision 1:
; - if everything's functional and new values keep getting returned, most likely, an old prototype can't be found
; - if prototype are functionally equivalent, a base prototype doesn't have to always have the same identity
; - don't worry at all about keeping references to "parent prototypes"

; Todo 1: move 'cut' to it's own file
; Todo 2: allow metaproperties for getter and setters

(provide 'prototypes)
(require 'srfi-1) ; lists
(require 'srfi-23) ; errors
(require 'srfi-26) ; cut
(require 'let-values)
(require 'format)

(define rest cdr)

(define (const x)
  (lambda (_) x))

(define (compose . functions)
  (lambda (value)
    (cond
      ((null? functions) value)
      (else
        ((apply compose (rest functions))
          ((first functions) value))))))

(define-syntax when
  (syntax-rules ()
    ((when condition body ...)
      (cond
        (condition body ...)
        (else #f)))))

(define-syntax :
  (syntax-rules ()
    ((: (key) prototype)
      (: key prototype))
    ((: (first-key rest-keys ...) prototype)
      (: (rest-keys ...) (: first-key prototype)))
    ((: key prototype)
      (lookup 'key prototype))))

(define-syntax !
  (syntax-rules ()
    ((! (key) prototype value)
      (! key prototype value))
    ((! (first-key rest-keys ...) prototype value)
      (&! first-key prototype
        (lambda (inner) (! (rest-keys ...) inner value))))
    ((! key prototype value)
      (replace 'key prototype value))))

(define-syntax &!
  (syntax-rules ()
    ((&! (key) prototype function)
      (&! key prototype function))
    ((&! (first-key rest-keys ...) prototype function)
      (&! first-key prototype
        (lambda (inner) (&! (rest-keys ...) inner function))))
    ((&! key prototype function)
      (! key prototype (function (: key prototype))))))

(define-syntax @
  (syntax-rules ()
    ((@ key prototype args ...)
      (let ((computed-prototype prototype))
        ((: key computed-prototype) computed-prototype args ...)))))

(define-syntax make
  (syntax-rules ()
    ((make properties ...) (extend '() properties ...))))

(define-syntax extend
  (syntax-rules ()
    ((extend prototype properties ...)
      (extend-procedural prototype
        (convert-properties properties ...)))))

(define-syntax convert-properties
  (syntax-rules ()
    ((convert-properties)
      '())
    ((convert-properties ((key arguments ...) body ...) tail ...)
      (cons
        (cons 'key (lambda (arguments ...) (begin body ...)))
        (convert-properties tail ...)))
    ((convert-properties (key value) tail ...)
      (cons
        (cons 'key value)
        (convert-properties tail ...)))))

(define (make-procedural properties)
  (extend-procedural '() properties))

(define (extend-procedural prototype properties)
  (cond
    ((null? properties) prototype)
    (else
      (let* (
        (next (first properties))
        (key (car next))
        (value (cdr next))
        )
        (extend-procedural
          (cond
            ((has? key prototype)
              (replace key prototype value))
            (else
              (cons next prototype)))
          (rest properties))))))

(define (error-property-not-found key)
  (error (format "Property not found: ~A" key)))

(define (has? key prototype)
  (let ((found (assq key prototype)))
    (case found
      ((#f) #f)
      (else #t))))

(define (lookup key prototype)
  (let ((found (assq key prototype)))
    (case found
      ((#f) (error-property-not-found key))
      (else (cdr found)))))

(define (replace key prototype value)
  (let-values
    (((start end)
      (break
        (compose car (cut eq? <> key))
        prototype)))
    (when (null? end)
      (error-property-not-found key))
    (cons
      (cons key value)
      (append start (rest end)))))

; If any other prototype (including a parent) happens
; to be using this same key-value cons cell, it's
; value gets replaced too
(define (replace! key prototype value)
  (let ((found (assq key prototype)))
    (when (not found)
      (error-property-not-found key))
    (set-cdr! found value)))

(define (prototypes-test)
  (let* (
    (x (make
      (a 1)
      (b 2)
      (c 3)
      (d 4)))
    (y (extend x
      (e 5)
      (f 6)))
    (z (extend y
      ((g self)
        (+ (: a self) 10))
      ((h self x)
        (display "adding") (newline)
        (! a self (+ (: a self) x)))))
    )
    (display x) (newline)
    (display y) (newline)
    (display z) (newline)
    (display (@ h z 50)) (newline)))

(define (prototypes-test2)
  (let* (
    (x (make
      (a 3)))
    (y x)
    (y2 (! a y 9))
    (z (extend x
      (a 30)))
    )
    (display x) (newline)
    (display y) (newline)
    (display y2) (newline)
    (display z) (newline)))

(define (prototypes-test3)
  (let* (
    (n (make
      (x (make
        (y 4)))))
    (n2 (! x n 0))
    (n3 (! (x y) n 10))
    (n4 (&! (x y) n (cut * <> 2)))
    )
    (display n) (newline)
    (display (: x n)) (newline)
    (display (: (x y) n)) (newline)
    (display n2) (newline)
    (display n3) (newline)
    (display n4) (newline)))

; vim: autoindent:softtabstop=2:shiftwidth=2:
