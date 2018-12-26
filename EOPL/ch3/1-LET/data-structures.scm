(module ds-env-val (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  ;;====================================== Expressed Value | Denoted Value
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?)))

  ;; expval -> number
  (define (expval->num expv)
    (cases ExpVal expv
      ($num-val (n)
                n)
      ($bool-val (b)
                 (eopl:error "Can't get num-val from boolen ExpVal"))))
  ;; expval -> boolean
  (define (expval->bool expv)
    (cases ExpVal expv
      ($num-val (n)
                (eopl:error "Can't get bool-val from num ExpVal"))
      ($bool-val (b)
                 b)))

  ;;====================================== Env (采用datatype 表示法)
  (define-datatype Env Env?
    ($empty-env)
    
    ($extend-env
     (var symbol?)
     (val ExpVal?)  ;注意：这里的val类型为ExpVal
     (env Env?)))

  (define (init-env)
    ($extend-env 'i ($num-val 1)
                 ($extend-env 'v ($num-val 5)
                              ($extend-env 'x ($num-val 10)
                                           ($empty-env)))))

  (define (apply-env env var)
    (cases Env env
      ($empty-env ()
                  (eopl:error 'apply-env "Empty env while search ~s" var))
      ($extend-env (saved-var saved-val saved-env)
                   (if (eqv? saved-var var)
                       saved-val
                       (apply-env saved-env var)))))
  )
