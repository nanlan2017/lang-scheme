(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  ;;============================================================= procedure  
  ;; apply-procedure : Proc * Val * Cont -> FinalAnswer
  (define (apply-procedure/k proc arg cont)
    (cases Proc proc
      ($procedure (param bodyexp env)
                  (eval/k bodyexp ($extend-env param arg env) cont))))

  ;;============================================================= env
  ;; apply-env == look-up-env
  (define (apply-env env var)
    (cases Env env
      ($empty-env ()
                  (eopl:error 'apply-env "Didn't find in env while search : ~s" var))
      ($extend-env (saved-var saved-val saved-env)
                   (if (eqv? saved-var var)
                       saved-val
                       (apply-env saved-env var)))
      ($extend-env-rec (p-name b-var p-body saved-env)
                       (if (eqv? var p-name)
                           ($proc-val ($procedure b-var p-body env))
                           (apply-env saved-env var)))
      ))
  ;;============================================================ Continuation
  (define-datatype Continuation Continuation?
    ($end-cont)
    ; zero?-exp
    ($zero?-exp-cont
     (cont Continuation?))
    ; let-exp
    ($let-exp-cont
     (var identifier?)
     (body expression?)
     (env Env?)
     (cont Continuation?))
    ; if-exp
    ($if-test-cont
     (then-exp expression?)
     (else-exp expression?)
     (env Env?)
     (cont Continuation?))
    ; diff-exp
    ($diff1-cont
     (e2 expression?)
     (env Env?)
     (cont Continuation?))
    ($diff2-cont
     (v1 ExpVal?)
     (cont Continuation?))
    ; call-exp
    ($rator-cont
     (rand-exp expression?)
     (env Env?)
     (cont Continuation?))
    ($rand-cont
     (f-expval ExpVal?)
     (cont Continuation?))
    ; Exception
    ($try-cont
     (cvar identifier?)
     (handler-exp expression?)
     (env Env?)
     (cont Continuation?))
    ($raise1-cont
     (cont Continuation?))
    )

  ; type FinalAnswer = ExpVal
  ; apply-cont :: Cont -> ExpVal -> FinalAnswer
  (define (apply-cont k %%VAL) ; val : 上一步的计算结果
    (cases Continuation k
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 %%VAL)
      ($zero?-exp-cont (cont)
                       (apply-cont cont ($bool-val (zero? (expval->num %%VAL))))) ; VALUE : zero?(e)  中e的值
      ($let-exp-cont (var body env cont)
                     (eval/k body ($extend-env var %%VAL env) cont))          ; VALUE : let x = e 中e的值
      ($if-test-cont (then-exp else-exp env cont)
                     (if (expval->bool %%VAL)                                 ; VALUE : if e ..   中e的值
                         (eval/k then-exp env cont)
                         (eval/k else-exp env cont)))
      ($diff1-cont (e2 env cont)
                   (eval/k e2 env ($diff2-cont %%VAL cont)))                  ; VALUE : -(e1,e2)  中e1的值
      ($diff2-cont (v1 cont)
                   (apply-cont cont ($num-val (- (expval->num v1) (expval->num %%VAL))))); VALUE : -(e1,e2)  中e2的值
      ($rator-cont (rand-exp env cont)
                   (eval/k rand-exp env ($rand-cont %%VAL cont)))             ; VALUE : (e1 e2)   中e1的值
      ($rand-cont (f-expval cont)                                             
                  (let [(f (expval->proc f-expval))]
                    (apply-procedure/k f %%VAL cont)))                        ; VALUE : (e1 e2)   中e2的值
      ; Exception
      ($try-cont (cvar handler-exp env cont)
                 (apply-cont cont %%VAL))
      ($raise1-cont (cont)
                    (apply-handler %%VAL cont))
      ))

  ; apply-handler :: ExpVal x Cont -> FinalAnswer
  ; ██████████████████ 核心就是认识到：cont是数据化的“计算”、通过模式匹配获取某些计算、你可以自由跳转计算
  (define (apply-handler val k)
    (cases Continuation k
      ;
      ($try-cont (cvar handler-exp env cont)
                 (eval/k handler-exp ($extend-env cvar val env) cont))
      ($end-cont ()
                 (eopl:error "=x=x=x= Uncaught Exception!~s~n" val))
      ;
      ($zero?-exp-cont (cont)
                       (apply-handler val cont))
      ($let-exp-cont (var body env cont)
                     (apply-handler val cont))
      ($if-test-cont (then-exp else-exp env cont)
                     (apply-handler val cont))
      ($diff1-cont (e2 env cont)
                   (apply-handler val cont))          
      ($diff2-cont (v1 cont)
                   (apply-handler val cont))
      ($rator-cont (rand-exp env cont)
                   (apply-handler val cont))
      ($rand-cont (f-expval cont)                                             
                  (apply-handler val cont))
      ;
      ($raise1-cont (cont)
                    (apply-handler val cont))
      ))
    
  
  ;;============================================================= eval    
  ; eval :: Expression x Env x Cont -> FinalAnswer
  (define (eval/k exp env cont)
    (cases expression exp
      (const-exp (n)
                 (apply-cont cont ($num-val n)))
      (var-exp (x)
               (apply-cont cont (apply-env env x)))
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env))))
      (letrec-exp (pid b-var p-body letrec-body)
                  (eval/k letrec-body ($extend-env-rec pid b-var p-body env) cont))
      (zero?-exp (e1)
                 (eval/k e1 env ($zero?-exp-cont cont)))
      (let-exp (var e1 body)
               (eval/k e1 env ($let-exp-cont var body env cont)))

      (if-exp (e1 e2 e3)
              (eval/k e1 env ($if-test-cont e2 e3 env cont)))
      (diff-exp (e1 e2)
                (eval/k e1 env ($diff1-cont e2 env cont)))      
      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont)))
      ; Exception Handling
      (try-exp (exp1 cvar handler-exp)
               (eval/k exp1 env ($try-cont cvar handler-exp env cont)))
      (raise-exp (exp1)
                 (eval/k exp1 env ($raise1-cont cont)))
      ))

  ; eval-program :: Program -> FinalAnswer
  (define (eval-program prog)
    (cases program prog
      (a-program (expr)
                 (eval/k expr (init-env) ($end-cont)))))
  ; =============================================================  
  ; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))

  (define (pretty-interp src) 
    (eopl:printf "--------------~n~s~n~n~n~s~n--------------" src (interp src)))

  (define sp scan&parse)
  (define run interp)

  )
