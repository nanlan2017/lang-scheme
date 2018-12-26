(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  (require "utils.scm")
  ;;====================================== Expressed Value | Denoted Value
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ;; PROC
    ($proc-val
     (p proc?))
    ;; Ref
    ($ref-val
     (idx reference?))
    )

  ;; expval -> number
  (define (expval->num expval)
    (cases ExpVal expval
      ($num-val (n) n)
      (else (eopl:error "Can't get num-val from ExpVal :" expval))
      ))
  ;; expval -> boolean
  (define (expval->bool expval)
    (cases ExpVal expval
      ($bool-val (b) b)
      (else (eopl:error "Can't get bool-val from ExpVal :" expval))
      ))
  ;; expval -> Proc
  (define (expval->proc expval)
    (cases ExpVal expval
      ($proc-val (p) p)
      (else (eopl:error "Can't get proc-val from ExpVal :" expval))
      ))
  ;; expval -> Reference (integer?)
  (define (expval->ref expval)
    (cases ExpVal expval
      ($ref-val (r) r)
      (else (eopl:error "Can't get ref-val from ExpVal :" expval))
      ))
  ;;====================================== Proc (抽象类型)
  ; proc? :: SchemeVal -> Bool
  (define (proc? v)
    (procedure? v)) 
    
  ;;====================================== Env (采用datatype 表示法)
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val ExpVal?)
     (env Env?))
    ; rec
    ($extend-env-rec
     (p-name identifier?)
     (b-var identifier?)
     (body expression?)
     (env Env?))
    )

  (define (init-env)
    ($extend-env 'i ($num-val 1)
                 ($extend-env 'v ($num-val 5)
                              ($extend-env 'x ($num-val 10)
                                           ($empty-env)))))

  ; $extend-env* :: [symbol] x [ExpVal] -> Env
  (define ($extend-env* vars expvals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car expvals) env))]
          ($extend-env* (cdr vars) (cdr expvals) new-env))))
  
  (define ($extend-env-rec* fS paramS expS env)
    (if (null? fS)
        env
        (let [(new-env ($extend-env-rec (car fS) (car paramS) (car expS) env))]
          ($extend-env-rec* (cdr fS) (cdr paramS) (cdr expS) new-env))))
    
  ;;======================================  Store
  (define STORE 'uninitialized-store)

  (define (empty-store) '())

  (define (get-store) STORE)

  (define (initialize-store!)
    (set! STORE (empty-store)))

  (define reference? integer?)

  ;```````````````````````
  (define (newref val)
    (let [(next-ref (length STORE))]
      (set! STORE (append STORE (list val)))
      next-ref))

  (define (deref i)
    (list-ref STORE i))

  ;; 计算出新store,并set!到STORE
  ;; 无返回值
  (define (setref! idx val)
    (letrec [(setref-inner (lambda (sto i)
                             (cond
                               [(null? sto) (eopl:error "Invalid Reference!")]
                               [(zero? i) (cons val (cdr sto))]
                               [else (cons (car sto) (setref-inner (cdr sto) (- i 1)))])))]
      (set! STORE (setref-inner STORE idx))))

  )
