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
  
  ;;============================================================ Continuation
  (define-datatype Continuation Continuation?
    ($end-cont)
    ; zero?-exp
    ($zero?-exp-cont
     (cont Continuation?)
     (saved-try-cont Continuation?))
    ; let-exp
    ($let-exp-cont
     (var identifier?)
     (body expression?)
     (env Env?)
     (cont Continuation?)
     (saved-try-cont Continuation?))
    ; if-exp
    ($if-test-cont
     (then-exp expression?)
     (else-exp expression?)
     (env Env?)
     (cont Continuation?)
     (saved-try-cont Continuation?))
    ; diff-exp
    ($diff1-cont
     (e2 expression?)
     (env Env?)
     (cont Continuation?)
     (saved-try-cont Continuation?))
    ($diff2-cont
     (v1 ExpVal?)
     (cont Continuation?)
     (saved-try-cont Continuation?))
    ; call-exp
    ($rator-cont
     (rand-exp expression?)
     (env Env?)
     (cont Continuation?)
     (saved-try-cont Continuation?))
    ($rand-cont
     (f-expval ExpVal?)
     (cont Continuation?)
     (saved-try-cont Continuation?))
    ; ███ Exception
    ($try-cont
     (cvar identifier?)
     (handler-exp expression?)
     (env Env?)
     (cont Continuation?))
    ($raise1-cont
     (cont Continuation?))
    )

  ; apply-cont :: Cont -> ExpVal -> FinalAnswer
  (define (apply-cont k VAL)                            ; val : 上一步的计算结果
    (cases Continuation k
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 VAL)
      ($zero?-exp-cont (cont saved-try-cont)
                       (apply-cont cont ($bool-val (zero? (expval->num VAL)))))
      
      ($let-exp-cont (var body env cont saved-try-cont)
                     (eval/k body ($extend-env var VAL env) cont))
      
      ($if-test-cont (then-exp else-exp env cont saved-try-cont)
                     (if (expval->bool VAL)                           
                         (eval/k then-exp env cont)
                         (eval/k else-exp env cont)))
      
      ($diff1-cont (e2 env cont saved-try-cont)
                   (eval/k e2 env ($diff2-cont VAL cont (extract-try-cont cont))))
      
      ($diff2-cont (v1 cont saved-try-cont)
                   (apply-cont cont ($num-val (- (expval->num v1) (expval->num VAL)))))
      
      ($rator-cont (rand-exp env cont saved-try-cont)
                   (eval/k rand-exp env ($rand-cont VAL cont (extract-try-cont cont))))
      
      ($rand-cont (f-expval cont saved-try-cont)                                             
                  (let [(f (expval->proc f-expval))]
                    (apply-procedure/k f VAL cont)))                    
      ; Exception
      ($try-cont (cvar handler-exp env cont)
                 (apply-cont cont VAL))  ; "If the body returns normally, then VAL should be sent to the continuation of try-exp : namely 'cont'"
      ($raise1-cont (cont)
                    (apply-handler VAL cont)) ; 如果raise-exp被求值了，说明已经触发了异常，需要把抛出的VAL发送给cont中nearest的 handler-proc
      ))

  ; apply-handler :: ExpVal x Cont -> FinalAnswer
  ; ███ 核心就是认识到：cont是数据化的“计算”。通过模式匹配获取某些计算、你可以自由跳转计算
  (define (apply-handler VAL k)
    (cases Continuation k
      ;
      ($try-cont (cvar handler-exp env cont)
                 (eval/k handler-exp ($extend-env cvar VAL env) cont))
      ($end-cont ()
                 (eopl:error "======= Uncaught Exception!~s~n" VAL))
      ;
      ($raise1-cont (cont)
                    (apply-handler VAL cont))
      ;
      ($zero?-exp-cont (cont saved-try-cont)
                       (apply-handler VAL saved-try-cont))
      ($let-exp-cont (var body env cont saved-try-cont)
                     (apply-handler VAL saved-try-cont))
      ($if-test-cont (then-exp else-exp env cont saved-try-cont)
                     (apply-handler VAL saved-try-cont))
      ($diff1-cont (e2 env cont saved-try-cont)
                   (apply-handler VAL saved-try-cont))          
      ($diff2-cont (v1 cont saved-try-cont)
                   (apply-handler VAL saved-try-cont))
      ($rator-cont (rand-exp env cont saved-try-cont)
                   (apply-handler VAL saved-try-cont))
      ($rand-cont (f-expval cont saved-try-cont)                                             
                  (apply-handler VAL saved-try-cont))      
      ))

  (define (extract-try-cont k)
    (cases Continuation k
      ($try-cont (cvar handler-exp env cont) cont)
      ($end-cont () k)
      ;
      ($raise1-cont (cont) cont)
      ;
      ($zero?-exp-cont (cont saved-try-cont) saved-try-cont)
      ($let-exp-cont (var body env cont saved-try-cont) saved-try-cont)
      ($if-test-cont (then-exp else-exp env cont saved-try-cont) saved-try-cont)
      ($diff1-cont (e2 env cont saved-try-cont) saved-try-cont)          
      ($diff2-cont (v1 cont saved-try-cont) saved-try-cont)
      ($rator-cont (rand-exp env cont saved-try-cont) saved-try-cont)
      ($rand-cont (f-expval cont saved-try-cont) saved-try-cont)      
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
      ;
      (letrec-exp (pid b-var p-body letrec-body)
                  (eval/k letrec-body ($extend-env-rec pid b-var p-body env) cont))
      (zero?-exp (e1)
                 (eval/k e1 env ($zero?-exp-cont cont (extract-try-cont cont))))
      (let-exp (var e1 body)
               (eval/k e1 env ($let-exp-cont var body env cont (extract-try-cont cont))))
      (if-exp (e1 e2 e3)
              (eval/k e1 env ($if-test-cont e2 e3 env cont (extract-try-cont cont))))
      (diff-exp (e1 e2)
                (eval/k e1 env ($diff1-cont e2 env cont (extract-try-cont cont))))      
      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont (extract-try-cont cont))))
      
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
  (define run interp)

  )
