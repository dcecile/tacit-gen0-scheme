(require 'srfi-1) ; lists
(require 'srfi-23) ; errors
(require 'srfi-26) ; cut

(require 'prototypes)

;-------------------------------------
; Generic
;----------------

(define (append-map f l . o)
  (apply append (apply (cut map f l <...>) o)))

;-------------------------------------
; Construction
;----------------
; top level: (chain choose transform token)
(define (choose . paths)
  (append-map (lambda (x) (x)) paths))

(define (pass-through path)
  (transform
    (lambda (_) identity)
    path))

(define (chain spec)
  (let (
    (before (map pass-through (: before spec)))
    (use (transform (lambda (v) (lambda (_) v)) (: use spec)))
    (after (map pass-through (: after spec)))
    )
    (apply follow (append
      (list (pure 'void))
      before
      (list use)
      after))))

(define (transform function . paths)
  (cond
    ((= (length paths) 1)
      (follow
        (first paths)
        (pure (lambda (value) (function value)))))
    (else (let (
      (start (pure '()))
      (finish (pure (lambda (args)
        (apply function (reverse args)))))
      (middle (map
        (lambda (path)
          (transform
            (lambda (value) (lambda (args)
              (cons value args)))
            path))
        paths))
      )
      (apply follow (append
        (list start)
        middle
        (list finish)))))))


(define (token tok)
  (lambda ()
    (list
      (make
        (variant 'step)
        (require tok)
        (compute identity)
        (continue '())))))
        
(define (pure value) (lambda ()
  (list
    (make
      (variant 'end)
      (value value)))))

(define (follow x . y)
  (lambda ()
    (append-map (cut follow-one <> y) (x))))

(define (follow-one x y)
  (case (: variant x)
    ('end
      (follow-one-value y (: value x)))
    ('step
      (list (&! continue x (cut append <> y))))))

(define (follow-one-value x value)
  (cond
    ((null? x) ((pure value)))
    (else (let (
      (now ((first x)))
      (later (rest x))
      )
      (append-map
        (lambda (y)
          (case (: variant y)
            ('end
              (follow-one-value later ((: value y) value)))
            ('step
              (list (&! continue y (cut append
                <>
                (list (pure (lambda (function)
                  (function value))))
                later))))))
        now)))))

(define (make-paths x)
  (filter (lambda (y) (eq? (: variant y) 'step)) (x)))
      
(define test-follow-simple
  (follow
    (token 'a)
    (pure identity)))
(define test-follow-pure
  (follow
    (pure 3)
    (pure (cut * <> 2))
    (lambda ()
      (choose
        (transform (lambda (r) (lambda (p) (list p r)))
          (token 'a))
        (transform
          (lambda (r)
            (lambda (p)
              (list r p)))
          (token 'b))))))
(define test-follow-left-associative
  (follow
    (token 2)
    (pure (cut expt <> 3))
    (pure (cut expt <> 4))))
(define test-follow-nesting
  (follow
    (token 2)
    (follow
      (follow
        (token 3)
        (pure (cut + <> 1)))
      (pure (lambda (value) (lambda (previous) (expt previous value)))))))
(define test-transform-single
  (transform
    (lambda (r) (list r 1 2 3))
    (token 'a)))
(define test-transform-simple
  (transform
    list
    (token 'a)
    (token 'b)))
(define test-transform-simulate
  (follow
    (pure '())
    (follow
      (token 'a)
      (pure (lambda (v) (lambda (p) (cons v p)))))
    (follow
      (token 'b)
      (pure (lambda (v) (lambda (p) (cons v p)))))))
(define test-transform-complex
  (transform
    +
    (token 1)
    (token 2)
    (lambda ()
      (choose
        (token 3)
        (token 4)))
    (token 5)))
(define test-transform-choose
  (lambda ()
    (choose
      (transform
        cons
        (token 'a)
        (lambda ()
          (choose
            (transform
              cons
              (token 'b)
              (pure 'end3))
            (pure 'end2))))
      (pure 'end1))))
(define test-transform-follow
  (transform
    cons
    (token 'a)
    (transform
      cons
      (token 'b)
      (pure 'end1))))
(define test-chain-simple
  (chain (make
    (before (list (token 'a)))
    (use (token 'b))
    (after '()))))



;-------------------------------------
; Parsing
;----------------

; execution structure:
; - input list
; - available options
;   - ambiguous queued functions and input
;   - input range
;   - nested paths

(define test-path-single (list (make
  (variant 'step)
  (require 'a)
  (compute identity)
  (continue (list
    (lambda () (list (make
      (variant 'end)
      (value identity)))))))))

(define (test-path-simple)
  (define (parse-cons tok cont)
    (make
      (variant 'step)
      (require tok)
      ((compute input) (lambda (previous) (cons input previous)))
      (continue cont)))
  (list (make
    (variant 'step)
    (require 'a)
    (compute list)
    (continue (list
      (lambda () (list
        (parse-cons 'b '())
        (parse-cons 'c '())))
      (lambda () (list
        (parse-cons 'd '())
        (parse-cons 'e '())
        (make
          (variant 'end)
          (value identity))))
      (lambda () (list (make
        (variant 'end)
        (value (cut cons 'z <>)))))
      (lambda () (list
        (parse-cons 'f
          (list
            (lambda () (list (parse-cons 'g '())))))))
      (lambda () (list (make
        (variant 'end)
        (value reverse)))))))))
; (parse test-path-simple 'a '(b d e f g))

; test: (parse (list (make (matches 'a) (variant 'option))) 'a '() '(a b))
(define (continue-with path value continue)
  (case (: variant path) 
    ('step
      (list (&! continue path (cut append
        <>
        (list (pure (lambda (function)
          (function value))))
        continue))))
    ('end
      (let ((next-value ((: value path) value)))
        (cond
          ((null? continue) (list (! value path next-value)))
          (else (let (
            (now ((first continue)))
            (later (rest continue))
            )
            (append-map (cut continue-with <> next-value later) now))))))))

(define (apply-input path input)
  (let* (
    (continue (: continue path))
    (value ((: compute path) input))
    )
    (cond
      ((null? continue) (list (make (variant 'end) (value value))))
      (else (let* (
        (now ((first continue)))
        (later (rest continue))
        )
        (append-map (cut continue-with <> value later) now))))))

(define (execute-path path)
  (: value path))

(define (matches-input? path input)
  (eqv? (: require path) input))

(define (is-done? test)
  (eq? (: variant test) 'end))

(define (parse-iter paths input next)
  (let-values (
    ((ended-paths new-paths)
      (partition is-done?
        (append-map (cut apply-input <> input)
          (filter (cut matches-input? <> input)
            paths))))
    )
    (cond
      ((null? next)
        (cond
          ((null? ended-paths)
            (display "Unexpected end of input") (newline)
            (display "Expected: ")
              (display (map (lambda (x) (: require x)) paths))
              (display (map (lambda (x) (: require x)) new-paths))
              (newline)
            (error "Unable to parse"))
          (else
            (execute-path (first ended-paths)))))
      ((null? new-paths)
        (display "Unexpected: ") (display input) (newline)
        (display "Expected: ")
          (display (map (lambda (x) (: require x)) paths))
          (newline)
        (display "Unfinished input: ") (display next) (newline)
        (display "Finish values: ")
          (display (map (lambda (x) (: value x)) ended-paths))
          (newline)
        (error "Unable to parse"))
      (else
        (parse-iter new-paths (first next) (rest next))))))

(define (parse paths input)
  (parse-iter (make-paths paths) (first input) (rest input)))
(define (parse-string paths input)
  (parse paths (string->list input)))


;-------------------------------------
; Combinators
;----------------
(define (one-of tokens)
  (cond
    ((= 1 (length tokens))
      (token (first tokens)))
    (else (lambda ()
      (choose
        (token (first tokens))
        (one-of (rest tokens)))))))

(define (sep-by main sep) (lambda ()
  (choose
    (transform
      cons
      main
      (many
        (chain (make
          (before (list sep))
          (use main)
          (after '())))))
    (pure '()))))

(define (some tok)
  (transform
    cons
    tok
    (many tok)))

(define (many tok) (lambda ()
  (choose
    (some tok)
    (pure '()))))

(define (ignore . parsers)
  (chain (make
    (before parsers)
    (use (pure 'void))
    (after '()))))

(define test-many-simple
  (transform
    append
    (many (token 'a))
    (some (lambda ()
      (choose
        (token 'b)
        (token 'c))))
    (many (token 'd))))
(define test-sepby-simple
  (sep-by
    (token 'a)
    (many (token 'b))))
(define test-oneof-simple
  (one-of '(a b c d)))

;-------------------------------------
; Text
;----------------
(define (keyword tokens)
  (cond
    ((null? tokens) (pure 'void))
    (else
      (ignore
        (token (first tokens))
        (keyword (rest tokens))))))

(define whitespace
  (many (token #\space)))

; vim: autoindent:softtabstop=2:shiftwidth=2:
