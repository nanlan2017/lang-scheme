(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  ;;============================================================= procedure
  ; ★ procedural-representation Under "apply-procedure"
  ;; procedure : Var * Exp * Env -> Proc
  (define ($procedure var body env)
    (lambda (arg)
      (eval body ($extend-env var arg env))))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  (define (apply-procedure proc arg)
    (proc arg))
  
  ;;============================================================= 
  ;; eval :: expression x Env -> ExpVal
  (define (eval exp env)
    (cases expression exp
      (const-exp (n)
                 ($num-val n))
      (var-exp (x)
               (apply-env env x))
      (diff-exp (e1 e2)
                ($num-val (- (expval->num (eval e1 env))
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
                 (eval body ($extend-env var v1 env))))
      ;; proc
      (proc-exp (var body)
                ($proc-val ($procedure var body env)))
      (call-exp (rator rand)
                (let [(f (expval->proc (eval rator env)))
                      (arg (eval rand env))]
                  (apply-procedure f arg)))
                  
      ))

  ;; eval-program :: Program -> ExpVal
  (define (eval-program prog)
    (cases program prog
      (a-program (expr)
                 (eval expr (init-env)))))
  ;;=============================================================  
  ;; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))

  (define (pretty-interp src) 
    (eopl:printf "--------------~n~s~n~n~n~s~n--------------" src (interp src)))

  )
