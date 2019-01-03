(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  ;;============================================================= procedure  
  ;; apply-procedure : Proc * Val -> ExpVal
  (define (apply-procedure proc arg)
    (cases Proc proc
      ($procedure (param bodyexp env)
                  (eval bodyexp ($extend-env param arg env)))))

  ;;=============================================================
  ; eval-module-body :: ModuleBody<#[VarDefinitions]> * Env -> TypedModule<#bindings = extend-env>
  (define (eval-module-body m-body env)
    (cases ModuleBody m-body
      ($a-module-body (var-defs)
                      (if (null? var-defs)
                          ($a-simple-module env)
                          (cases VarDefinition (car var-defs)
                            ($a-var-definition (var exp)
                                               (let [(val (eval exp env))]
                                                 (eval-module-body ($a-module-body (cdr var-defs)) ($extend-env var val env)))))))))
               

  ; add-module-defns-to-env :: [ModuleDefinition] * Env -> Env
  (define (add-module-defns-to-env defns env)
    (if (null? defns)
        env
        (cases ModuleDefinition (car defns)
          ($a-module-definition (m-name iface m-body)
                                (let [(new-env ($extend-env-with-module m-name
                                                                        (eval-module-body m-body env)
                                                                        env))]
                                  (add-module-defns-to-env (cdr defns) new-env))))))
  
  ;;============================================================= 
  ;; eval :: expression x Env -> ExpVal
  (define (eval exp env)
    (cases Expression exp
      ($const-exp (n)
                  ($num-val n))
      ($var-exp (x)
                (apply-env env x))
      ($diff-exp (e1 e2)
                 (let [(num1 (expval->num (eval e1 env)))
                       (num2 (expval->num (eval e2 env)))]
                   ($num-val (- num1 num2))))
      ($zero?-exp (e1)
                  (let [(v1 (expval->num (eval e1 env)))]
                    (if (zero? v1)
                        ($bool-val #t)
                        ($bool-val #f))))
      ($if-exp (e1 e2 e3)
               (if (expval->bool (eval e1 env))
                   (eval e2 env)
                   (eval e3 env)))
      ($let-exp (var e1 body)
                (let [(v1 (eval e1 env))]
                  (eval body ($extend-env var v1 env))))
      ;; 1-arg proc
      ($proc-exp (var ty body)
                 ($proc-val ($procedure var body env)))
      ($call-exp (rator rand)
                 (let [(f (expval->proc (eval rator env)))
                       (arg (eval rand env))]
                   (apply-procedure f arg)))                               
      ;; letrec
      ($letrec-exp (p-res-type pid b-var b-var-type p-body letrec-body)
                   (eval letrec-body ($extend-env-rec pid b-var p-body env)))
      ;; 
      ($qualified-var-exp (mod-name var-name)
                          (lookup-qualified-var-in-env mod-name var-name env))
                  
      ))

  ; eval-program :: Program -> ExpVal
  (define (eval-program prog)
    (cases Program prog
      ($a-program (mod-defs expr)
                  (eval expr (add-module-defns-to-env mod-defs (init-env))))))
  ; =============================================================  
  ; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))

  (define (pretty-interp src) 
    (eopl:printf "--------------~n~s~n~n~n~s~n--------------" src (interp src)))

  
  (define run interp)

  )
