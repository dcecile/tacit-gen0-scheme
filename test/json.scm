(require 'srfi-1) ; lists
(require 'srfi-23) ; errors
(require 'srfi-26) ; cut

(require 'prototypes)
(require 'synops)

;-------------------------------------
; Json data
;----------------
(define make-number identity)
(define make-array identity)

;-------------------------------------
; Json parsers
;----------------
(define json-value (chain (make
  (before '())
  (use (lambda ()
    (choose
      json-number
      json-true
      json-array)))
  (after (list
    whitespace)))))

(define json-array (chain (make
  (before (list
    (token #\[) whitespace))
  (use
    (transform
      make-array
      (sep-by
        json-value
        (ignore
          (token #\,)
          whitespace))))
  (after (list
    (token #\]))))))

(define json-number
  (transform
    make-number
    (some
      (one-of (string->list "0123456789-.e")))))

(define json-true (chain (make
  (before (list
    (keyword (string->list "true"))))
  (use
    (pure 'true))
  (after '()))))


(define (test-json-simple)
  (parse-string json-value "[0302, 4, [[true]]]"))

; vim: autoindent:softtabstop=2:shiftwidth=2:

