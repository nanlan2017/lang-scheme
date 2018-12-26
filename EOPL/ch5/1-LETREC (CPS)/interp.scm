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
    
    ($zero?-cont
     (val ExpVal?)
     (cont Continuation?))
        
    )

  ; type FinalAnswer = ExpVal
  ; apply-cont :: Cont -> ExpVal -> FinalAnswer
  (define (apply-cont k val)
    (cases Continuation k
      ($end-cont ()
                 (eopl:printf "End of Computation.~%")
                 val)
      ($zero?-cont (expval cont)
                   (apply-cont cont ($bool-val (zero? (expval->num expval)))))
                   
      ))  
  
  ;;============================================================= eval
  ; eval :: Expression x Env x Cont -> FinalAnswer
  ; 重写目标： 所有对 eval/k的调用都属于tail call (doesn't build control context)
  (define (eval/k exp env cont)
    (cases expression exp
      ;--------------------------------------------- no call on eval/k
      ;      (const-exp (n)
      ;                 ($num-val n))
      ;      (var-exp (x)
      ;               (apply-env env x))
      ;      (proc-exp (var body)
      ;                ($proc-val ($procedure var body env)))
      (const-exp (n)
                 (apply-cont cont ($num-val n)))
      (var-exp (x)
               (apply-cont cont (apply-env env x)))
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env))))
      ;--------------------------------------------- 1 call on eval
      ; █████████ f(x,cont)  !=  cont( f(x,end-cont) )
      
      ; letrec : the inner-'eval' is in the same cont as caller-eval
      
      ;      (letrec-exp (pid b-var p-body letrec-body)
      ;                  (eval letrec-body ($extend-env-rec pid b-var p-body env)))
      (letrec-exp (pid b-var p-body letrec-body)
                  (eval/k letrec-body ($extend-env-rec pid b-var p-body env) cont))
      
      ; zero?-exp : eval --> [ expval->num >>> if zero?($) then ..else .. ] --> cont
      
      ;      (zero?-exp (e1)
      ;                 (let [(v1 (expval->num (eval e1 env)))]
      ;                   (if (zero? v1)
      ;                       ($bool-val #t)
      ;                       ($bool-val #f))))
      (zero?-exp (e1)
                 (eval/k e1 env ($zero?-cont cont)))
      ;--------------------------------------------- 2 call on eval
      ;      (diff-exp (e1 e2)
      ;                (let [(num1 (expval->num (eval e1 env)))
      ;                      (num2 (expval->num (eval e2 env)))]
      ;                  ($num-val (- num1 num2))))
      (diff-exp (e1 e2)
                (let [(num1 (expval->num (eval e1 env)))
                      (num2 (expval->num (eval e2 env)))]
                  ($num-val (- num1 num2))))
      
      ;      (let-exp (var e1 body)
      ;               (let [(v1 (eval e1 env))]
      ;                 (eval body ($extend-env var v1 env))))
      (let-exp (var e1 body)
               (let [(v1 (eval e1 env))]
                 (eval body ($extend-env var v1 env))))
      
      ;      (call-exp (rator rand)
      ;                (let [(f (expval->proc (eval rator env)))
      ;                      (arg (eval rand env))]
      ;                  (apply-procedure f arg)))
      (call-exp (rator rand)
                (let [(f (expval->proc (eval rator env)))
                      (arg (eval rand env))]
                  (apply-procedure f arg)))
      ;--------------------------------------------- 3 call on eval
      ;      (if-exp (e1 e2 e3)
      ;              (if (expval->bool (eval e1 env))
      ;                  (eval e2 env)
      ;                  (eval e3 env)))
      (if-exp (e1 e2 e3)
              (if (expval->bool (eval e1 env))
                  (eval e2 env)
                  (eval e3 env)))
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
