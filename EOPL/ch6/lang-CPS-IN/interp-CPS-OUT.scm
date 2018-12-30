(module interp-CPS-OUT (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang-CPS-OUT.scm")
  (require "data-structures.scm")
  ;====================================================================
  (define (apply-cont cont val)
    (cases continuation cont
      (end-cont ()
                val)))

  (define (apply-procedure/k proc1 args cont)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (★eval/k body (extend-env* vars args saved-env) cont))))
  ;====================================================================
  (define eval-program 
    (lambda (pgm)
      (cases cps-out-program pgm
        (cps-a-program (exp1)
                       (★eval/k exp1 (init-env) (end-cont))))))

  ; 和最开始的naive的内容一样
  (define ☆eval-simple
    (lambda (exp env)
      (cases simple-expression exp
        (cps-const-exp (num) (num-val num))
        
        (cps-var-exp (var) (apply-env env var))

        (cps-diff-exp (exp1 exp2)
                      (let [(val1 (expval->num (☆eval-simple exp1 env)))
                            (val2 (expval->num (☆eval-simple exp2 env)))]
                        (num-val (- val1 val2))))

        (cps-zero?-exp (exp1)
                       (bool-val (zero? (expval->num (☆eval-simple exp1 env)))))

        (cps-sum-exp (exps)
                     (let ((numbers (map (lambda (exp) (expval->num (☆eval-simple exp env))) exps)))
                       (num-val
                        (let sum-loop ((nums numbers)) ; 递归函数的便捷写法
                          (if (null? nums)
                              0
                              (+ (car nums) (sum-loop (cdr nums))))))))

        (cps-proc-exp (vars body)
                      (proc-val (procedure vars body env)))

        )))

  ; ★eval/k :: TfExp -> Env -> Cont -> FinalAnswer(ExpVal)
  ; 因为都是tail-form expression，所以cont都是一样的！
  (define ★eval/k
    (lambda (exp env XCONT)
      (cases tfexp exp
        (simple-exp->exp (simple)
                         (apply-cont XCONT (☆eval-simple simple env)))
        
        (cps-let-exp (var rhs body)
                     (let ((val (☆eval-simple rhs env)))
                       (★eval/k body (extend-env* (list var) (list val) env) XCONT)))
        
        (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
                        (★eval/k letrec-body (extend-env-rec** p-names b-varss p-bodies env) XCONT))
        
        (cps-if-exp (simple1 body1 body2)
                    (if (expval->bool (☆eval-simple simple1 env))
                        (★eval/k body1 env XCONT)
                        (★eval/k body2 env XCONT)))
        
        (cps-call-exp (rator rands)
                      (let ((rator-proc (expval->proc (☆eval-simple rator env)))
                            (rand-vals (map (lambda (simple) (☆eval-simple simple env)) rands)))
                        (apply-procedure/k rator-proc rand-vals XCONT)))
        )))

  )
