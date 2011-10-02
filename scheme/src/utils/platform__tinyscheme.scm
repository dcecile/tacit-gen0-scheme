(define-macro (define-platform-yes name)
  `(define-macro (,name body)
    body))

(define-macro (define-platform-no name)
  `(define-macro (,name body)
    '()))

(define-platform-yes
  platform-tinyscheme?)

(define-platform-no
  platform-chicken?)

(define-platform-no
  platform-racket?)

(define-platform-no
  platform-define-syntax?)
