(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  ;;============================================================ Proc (part 2)
  ;; 依赖于 eval
  ;; apply-procedure : Proc * [ExpVal] -> ExpVal
  (define (apply-procedure proc arg)
    (cases Procedure proc
      ($procedure (var body env)
                  (eval body ($extend-env var ($ref-val (newref arg)) env)))))
  ;;============================================================= eval
  ;; eval :: expression x Env -> ExpVal
  (define (eval exp env)
    (cases expression exp
      (const-exp (n)
                 ($num-val n))
      (var-exp (x)
               (deref (denval->ref (apply-env env x))))
      (diff-exp (e1 e2)
                ($num-val (- (expval->num (eval e1 env))
                             (expval->num (eval e2 env)))))
      (add-exp (e1 e2)
               ($num-val (+ (expval->num (eval e1 env))
                            (expval->num (eval e2 env)))))
      (mult-exp (e1 e2)
                ($num-val (* (expval->num (eval e1 env))
                             (expval->num (eval e2 env)))))
      (zero?-exp (e1)
                 (let* [(v1 (eval e1 env))
                        (v2 (expval->num v1))]
                   ($bool-val (if (= 0 v2) #t #f))))
      (if-exp (e1 e2 e3)
              (let [(v1 (eval e1 env))]
                (if (expval->bool v1)
                    (eval e2 env)
                    (eval e3 env))))
      (let-exp (var e1 body)
               (let [(v1 (eval e1 env))]
                 (eval body ($extend-env var ($ref-val (newref v1)) env))))

      ; 1-parameter procedure
      (proc-exp (var body)
                ($proc-val ($procedure var body env)))
      (call-exp (rator rand)
                (let [(f (expval->proc (eval rator env)))                  ;; 此处f 捆绑的env中并没有保存自身，对其body求值时无法找到自身的定义
                      (arg (eval rand env))]
                  (apply-procedure f arg)))                               
      ;; letrec*
      (letrec*-exp (pid-s bvar-s pbody-s letrec-body)
                   (eval letrec-body ($extend-env-rec pid-s bvar-s pbody-s env)))   ;; 区别在于：求let-body时要把f的定义压入Env
      (begin-exp (exps)
                 (let [(vals (map (lambda (e) (eval e env)) exps))]
                   (list-last vals)))
      (assign-exp (var exp1)
                  (begin  ;; Env中记录的都是RefVal
                    (setref! (denval->ref (apply-env env var)) (eval exp1 env))
                    'any-value))
                  
      ))

  ;; eval-program :: Program -> ExpVal
  (define (eval-program prog)
    (initialize-store!)
    (cases program prog
      (a-program (expr)
                 (eval expr (init-env)))))
  ;;=============================================================  
  ;; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))

  (define (pretty-interp src) 
    (eopl:printf "--------------~n~s~n~n~n~s~n--------------" src (interp src)))
  ;; short names
  (define sp scan&parse)
  (define run interp)

  )
