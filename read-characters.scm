(define-record-type
  letterq
  (letter tag data line column)
  is-letter?
  (tag letter-tag)
  (data letter-data)
  (line letter-line)
  (column letter-column))

(define (reduce seed proc list)
  (cond
    ((null? list)
      seed)
    (else
      (proc
        seed
        (car list)
        (lambda (seed proc)
          (reduce seed proc (cdr list)))))))

(define (any? proc? list)
  (define (any? seed next continue)
    (define result (proc? next))
    (cond
      (result
        result)
      (else
        (continue #f any?))))
  (reduce
    #f
    any?
    list))

(define (all? proc? list)
  (define (all? seed next continue)
    (define result (proc? next))
    (cond
      (result
        (continue result all?))
      (else
        #f)))
  (reduce
    #f
    all?
    list))

(define (string-range from to)
  (define (range progress from to)
    (cond
      ((> from to)
        progress)
      (else
        (range
          (cons to progress)
          from
          (- to 1)))))
  (define integers
    (range
      '()
      (char->integer from)
      (char->integer to)))
  (list->string
    (map integer->char integers)))

(define letter-mapping `(
  (newline "\n")
  (return "\r")
  (space " ")
  (dot ".")
  (block ":")
  (comment "%")
  (open "[{(")
  (close ")}]")
  (string "'\"")
  (identifier
    "/_+*-"
    ,(string-range #\a #\z)
    ,(string-range #\A #\Z)
    ,(string-range #\0 #\9))))

(define letter-mapping-functions
  (map
    (lambda (mapping)
      (define tag (car mapping))
      (define tag-chars
        (apply
          append
          (map string->list (cdr mapping))))
      (define (test? next)
        (any?
          (lambda (tag-char) (char=? tag-char next))
          tag-chars))
      (lambda (next)
        (cond
          ((test? next)
            tag)
          (else
            #f))))
    letter-mapping))

(define (categorize next)
  (or
    #f
    (any?
      (lambda (test?) (test? next))
      letter-mapping-functions)
    'illegal))

(define (read-one line column)
  (lambda (next continue)
    (define tag (categorize next))
    (continue
      (letter tag next line column)
      (cond
        ((eq? tag 'newline)
          (read-one (+ line 1) 0))
        ((eq? tag 'return)
          (read-one line column))
        (else
          (read-one line (+ column 1)))))))

(define (map-reduce-char-from-port port proc)
  (define (map-reduce list proc)
    (define next (read-char port))
    (cond
      ((eof-object? next)
        list)
      (else
        (proc
          next
          (lambda (result proc)
            (map-reduce
              (cons result list)
              proc))))))
  (reverse
    (map-reduce
      '()
      proc)))


(define (read-all port)
  (map-reduce-char-from-port
    port
    (read-one 0 0)))

(define (read-file file)
  (call-with-input-file
    file
    read-all))

(define (read-string x) (read-all (open-input-string x)))

(define (test)
  (read-string "ab c\ne."))

(define (test2)
  (define (x seed)
    (lambda (next continue)
      (define seed2 (cons next seed))
      (continue
        (list->string seed2)
        (x seed2))))
  (map-reduce-char-from-port (open-input-string "abc") (x '())))

