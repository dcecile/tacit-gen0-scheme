(platform-tinyscheme?
  (use ./prototype__tinyscheme))

(platform-define-syntax?
  (use ./prototype__define-syntax))

(def (utils/get-property object member)
  (cond
    ((list? object)
      (def found (assq member object))
      (cond
        (found
          (cdr found))
        (else
          (error "property not found:" member))))
    (else
      (error "invalid property object" object member))))

(def (:& value . props)
  (foldr
    (lambda (value prop)
      (utils/get-property prop value))
    value
    props))

(def (has?& object prop)
  (any->boolean
    (assq prop object)))
