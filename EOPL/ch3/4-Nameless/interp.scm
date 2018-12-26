(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  ;;============================================================= procedure
  (define ($procedure body env)
    (lambda (arg)
      (eval body (extend-env arg env))))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  (define (apply-procedure proc arg)
    (proc arg))
  
  ;;============================================================= trans
  ;; ★★★     _0, _1 分别对应该处的 senv 中的第0个符号、第1个符号

  
  ;; trans :: expression x SEnv -> Nameless-Exp
  ;; 6+2 
  (define (trans exp senv)
    (cases expression exp
      (const-exp (n)
                 (const-exp n))      
      (diff-exp (e1 e2)
                (diff-exp (trans e1 senv) (trans e2 senv)))
      (zero?-exp (e1)
                 (zero?-exp (trans e1 senv)))
      (if-exp (e1 e2 e3)
              (if-exp (trans e1 senv) (trans e2 senv) (trans e3 senv)))      
      (call-exp (rator rand)
                (call-exp (trans rator senv) (trans rand senv)))
      ;;`````````````````````````` Node -> Nameless Node
      (var-exp (x)
               (nameless-var-exp (apply-senv senv x)))
      (let-exp (var e1 body)
               (nameless-let-exp (trans e1 senv)
                                 (trans body (extend-senv var senv))))           ; let 会创建新binding
      (proc-exp (var body)
                (nameless-proc-exp (trans body (extend-senv var senv))))         ; proc 会创建新binding (declaration)
      ;;``````````````````````````
      (nameless-var-exp (idx)
                        (eopl:error "Nameless var shouldn't appear in trans"))
      (nameless-let-exp (exp1 body)
                        (eopl:error "Nameless let shouldn't appear in trans"))
      (nameless-proc-exp (body)
                         (eopl:error "Nameless proc shouldn't appear in trans"))
      ))

  (define (trans-program prog)
    (cases program prog
      (a-program (expr)
                 (a-program (trans expr (init-senv))))))
  ;; nameless的AST
  (define (namelessfy src)
    (trans-program (scan&parse src)))
    
    
  ;;============================================================= eval
  ;; ★★★     _0, _1 分别去用 env中对应索引的值
  
  ;; ★ SEnv = [i, v ,x ]
  ;; ★  Env = [1, 5 ,10]
  
  ;; eval :: Nameless-Exp x Env -> ExpVal
  (define (eval exp env)
    (cases expression exp
      (const-exp (n)
                 ($num-val n))      
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
      (call-exp (rator rand)
                (let [(f (expval->proc (eval rator env)))
                      (arg (eval rand env))]
                  (apply-procedure f arg)))
      ;;``````````````````````````
      (var-exp (x)
               (eopl:error "Non-Nameless var shouldn't appear in eval"))
      (let-exp (var e1 body)
               (eopl:error "Non-Nameless let shouldn't appear in eval"))
      (proc-exp (var body)
                (eopl:error "Non-Nameless proc shouldn't appear in eval"))
      
      ;;``````````````````````````
      (nameless-var-exp (idx)
                        (apply-env env idx))
      (nameless-let-exp (exp1 body)        ;; 新增binding
                        (let [(v1 (eval exp1 env))]
                          (eval body (extend-env v1 env))))
      (nameless-proc-exp (body)
                         ($proc-val ($procedure body env)))  ;; Wrap成ExpVal                  
      ))

  ;; eval-program :: Nameless-Program -> ExpVal
  (define (eval-program prog)
    (cases program prog
      (a-program (nameless-expr)
                 (eval nameless-expr (init-env)))))
   
  ;; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (namelessfy src)))
  )
