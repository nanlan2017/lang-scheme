(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  ;============================================================= env
  ; apply-env == look-up-env
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
  ;============================================================ Continuation (part 1)
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
    )  
  ;============================================================= procedure
  ; ████████████████████████████████████  什么也没改！就是给 eval-program 加了个 trampoline!!
  ; trampoline = "calc-if-not-ended"
  ; █████████ type Bounce = ExpVal | ()->Bounce
  
  ; trampoline :: Bounce -> FinalAnswer 
  ; 参数是一次递归调用的结果值：若为ExpVal,则说明递归已结束；否则是Thunk:进行下一次递归、继续判断...
  (define (trampoline bounce)
    (if (ExpVal? bounce)
        bounce          ; FinalAnswer
        (trampoline (bounce))))
  
  ; apply-procedure :: Proc x Val x Cont -> Bounce
  ; 注意： tail call on eval/k ,所以其返回类型必然同 eval/k的返回类型一致
  (define (apply-procedure/k proc arg cont)
    (cases Proc proc
      ($procedure (param bodyexp env)
                           ; (eval/k bodyexp ($extend-env param arg env) cont)
                  (lambda () (eval/k bodyexp ($extend-env param arg env) cont))  ; █████████
                  )))
  
  ; apply-cont :: Cont x ExpVal -> Bounce
  ; 注意： tail call on eval/k 或 apply-cont 或 apply-procedure/k
  (define (apply-cont k VALUE)
    (cases Continuation k
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 VALUE)
      ($zero?-exp-cont (cont)
                   (apply-cont cont ($bool-val (zero? (expval->num VALUE))))) ; VALUE : zero?(e)  中e的值
      ($let-exp-cont (var body env cont)
                     (eval/k body ($extend-env var VALUE env) cont))          ; VALUE : let x = e 中e的值
      ($if-test-cont (then-exp else-exp env cont)
                     (if (expval->bool VALUE)                                 ; VALUE : if e ..   中e的值
                         (eval/k then-exp env cont)
                         (eval/k else-exp env cont)))
      ($diff1-cont (e2 env cont)
                   (eval/k e2 env ($diff2-cont VALUE cont)))                  ; VALUE : -(e1,e2)  中e1的值
      ($diff2-cont (v1 cont)
                   (apply-cont cont ($num-val (- (expval->num v1) (expval->num VALUE))))); VALUE : -(e1,e2)  中e2的值
      ($rator-cont (rand-exp env cont)
                   (eval/k rand-exp env ($rand-cont VALUE cont)))             ; VALUE : (e1 e2)   中e1的值
      ($rand-cont (f-expval cont)                                             
                  (let [(f (expval->proc f-expval))]
                    (apply-procedure/k f VALUE cont)                          ; VALUE : (e1 e2)   中e2的值
                    ;错误： (apply-cont cont (apply-procedure f VALUE))
                    ))                                        
      ))  
  
  ;============================================================= eval/k    
  ; eval/k :: Expression x Env x Cont -> Bounce
  ; 注意：均为对 apply-cont / eval/k 的tail call
  (define (eval/k exp env cont)
    (cases expression exp
      ;--------------------- no call on eval/k
      (const-exp (n)
                 (apply-cont cont ($num-val n)))
      (var-exp (x)
               (apply-cont cont (apply-env env x)))
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env))))
      ;--------------------- call on eval
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
      ))

  ; eval-program :: Program -> FinalAnswer
  (define (eval-program prog)
    (cases program prog
      (a-program (expr)
                 (trampoline (eval/k expr (init-env) ($end-cont))))))
  ; =============================================================  
  ; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))

  (define (pretty-interp src) 
    (eopl:printf "--------------~n~s~n~n~n~s~n--------------" src (interp src)))

  (define sp scan&parse)
  (define run interp)

  )
