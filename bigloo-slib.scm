; For Bigloo 3.3a
; Run as "rlwrap bigloo -load bigloo-slib.scm"

; Bigloo bug, (let-values (((a b) (break (compose car (cut eq? <> 'a)) '((a 30))))) (list a b)) returns (() ()) not (() (a 30)), while call-with-values works fine

; test: (let-values (((a b) (values 1 2)) ((c d) (values 3 4))) (list a b c d))


; check: (expand '(let*-values (((a b) (values 1 2)) ((c d) (values a b))) (list a b c d)))
; check: (expand '(let-values (((a b) (values 1 2)) ((c d) (values 3 4))) (list a b c d)))

(putenv "SCHEME_LIBRARY_PATH" "/usr/lib/slib/")
(putenv "BIGLOO_IMPLEMENTATION_PATH" "/usr/lib/bigloo/3.3a/")

(load "/usr/lib/slib/bigloo.init")
(define macro:eval defmacro:eval)
(define macro:load defmacro:load)
(provide 'macro)
(provide 'values)
(library-load 'srfi1)
(provide 'srfi-1)

(define-syntax let-values
  (syntax-rules ()
    ((let-values bindings body ...)
      (let-values-mask bindings () (body ...)))))

; let-values-tmp bindings masks body
(define-syntax let-values-mask
  (syntax-rules ()
    ((let-values-mask () masks (body ...))
      (let masks body ...))
    ((let-values-mask ((args expr ...) bindings ...) masks body)
      (let-values-gen-masks
        args
        args
        ()
        (expr ...)
        (bindings ...)
        masks
        body))))

; let-values-gen-masks args to-mask masked-args
;   expr bindings masks body
(define-syntax let-values-gen-masks
  (syntax-rules ()
    ((let-values-gen-masks args () masked-args
        expr bindings masks body)
      (let-values-call
        args masked-args expr bindings masks body))
    ((let-values-gen-masks args (to-mask rest ...) (masked-args ...)
        expr bindings masks body)
      (let ((mask 'dummy))
        (let-values-gen-masks
          args
          (rest ...)
          (mask masked-args ...)
          expr bindings masks body)))))

(define-syntax let-values-call
  (syntax-rules ()
    ((let-values-call args masked-args (expr ...)
        bindings masks body)
      (call-with-values
        (lambda () expr ...)
        (lambda masked-args
          (let-values-join
            bindings
            args masked-args masks
            body))))))

; let-values-join bindings new-args new-masks mask body
(define-syntax let-values-join
  (syntax-rules ()
    ((let-values-join bindings () () mask body)
      (let-values-mask
        bindings
        mask
        body))
    ((let-values-join bindings
        (arg new-args ...) (mask new-masks ...) (masks ...)
        body)
      (let-values-join bindings (new-args ...) (new-masks ...)
        ((arg mask) masks ...) body))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body ...)
      (begin body ...))
    ((let*-values (((args ...) expr ...) bindings ...) body ...)
      (call-with-values
        (lambda () (begin expr ...))
        (lambda (args ...)
          (let*-values (bindings ...) body ...))))))
(provide 'let-values)

; vim: autoindent:softtabstop=2:shiftwidth=2:
