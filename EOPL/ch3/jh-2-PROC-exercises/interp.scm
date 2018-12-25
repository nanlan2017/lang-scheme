(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  ;;============================================================= procedure  
  ; 闭包表示： 便于变动该数据结构本身（闭包函数无需改变）
  ;    ex. 将Proc从单一var 提升为多参数vars 无需任何变动
  ; 结构表示： 便于提取其中的数据
  ;    ex. 假若Proc还有一个observer interface : size-of (parameters)
  
  ; ★ procedural-representation Under "apply-procedure"
  ;; procedure : [Var] * Exp * Env -> Proc
  (define ($procedure vars body env)
    (lambda (args)
      (let [(params-size (length vars))
            (args-size (length args))]
        (cond
          ; 调用的实参个数不足
          [(not (=  params-size args-size))  
           (let [(provided-vars (take vars args-size))
                 (left-vars (drop vars args-size))]
             (begin
;               (eopl:printf "Currying : Procedure with parameters : ~s ====> ~s ~n" vars args)
               (eval (proc-exp left-vars body) ($extend-env* provided-vars args env))))]  ;; 应返回一个Proc值(proc-exp手动构造)
          ; 调用的实参个数充分
          [else  (eval body ($extend-env* vars args env))]))))
  
  ;; apply-procedure : Proc * [ExpVal] -> ExpVal
  (define (apply-procedure proc args)
    (proc args))
  
  ;;============================================================= 
  ;; eval :: expression x Env -> ExpVal
  ; // 为什么结果类型不包括一个Env ?
  (define (eval exp env)
    (cases expression exp
      (const-exp (n)
                 ($num-val n))
      (var-exp (x)
               (apply-env env x))
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
                 (eval body ($extend-env var v1 env))))
      ;; proc
      ;; multi-args proc
      (proc-exp (vars body)
                ($proc-val ($procedure vars body env)))
      (call-exp (rator rands)
                (let [(f (expval->proc (eval rator env)))
                      (args (map (lambda (rand) (eval rand env)) rands))]
                  (apply-procedure f args)))
                  
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
