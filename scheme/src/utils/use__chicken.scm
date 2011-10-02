(use srfi-1)

(define foldl fold)

(define (utils/current-load-dir)
  (cond
    ((= (string-length ##sys#current-load-path) 0)
      "./")
    (else
      ##sys#current-load-path)))
