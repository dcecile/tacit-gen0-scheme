(define-syntax define-platform-yes
  (syntax-rules ()
    ((~ name)
      (define-syntax name
        (syntax-rules ()
          ((~ body)
            body))))))

(define-syntax define-platform-no
  (syntax-rules ()
    ((~ name)
      (define-syntax name
        (syntax-rules ()
          ((~ body)
            (void)))))))

(define-platform-no
  platform-tinyscheme?)

(define-platform-yes
  platform-chicken?)

(define-platform-no
  platform-racket?)

(define-platform-yes
  platform-define-syntax?)
