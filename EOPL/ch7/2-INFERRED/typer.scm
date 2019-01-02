(module typer (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "lang-type.scm")
  (require "unification.scm")  
  ; ========================================================================

  ; 例如：  $an-answer  int->t0  {t0 ~ bool}
  (define-datatype Answer Answer?
    ($an-answer
     (ty type?)
     (subst substitution?))
    )
  
  (define (type-of-program prog)
    (cases program prog
      (a-program (expr)
                 (cases Answer (██type-of expr (init-tenv) (empty-subst))
                   ($an-answer (ty subst)
                               (apply-subst-to-type ty subst))))))
    
  ; 又是两层架构:
  ;   x  ---> Env { x : ref 1 }           ---> Store {0x1 = 55 }
  ;   x  ---> TEnv{ x :: int -> t0 ...}   ---> Subst {t0 ~ bool}

  
  ;  ████████ For each kind of expression, we recur on the subexpressions, passing along the solution so far in the substitution argument.
  ;  Then we generate the equations for the current expression, according to the specification, and record these in the substitution by calling ░░unify.
  

  ; typeof :: Exp * TyEnv * Subst -> Answer (该Exp的类型、迄今unify出的substitions)
  (define (██type-of EXP TENV SUBST)
    (cases expression EXP
      (const-exp (n)
                 ($an-answer (int-type) SUBST))
      (var-exp (var)
               ($an-answer (apply-tenv TENV var) SUBST))
      ; --------------------------------------------------------------------------------
      (diff-exp (e1 e2)
                ; 1. T(e1) == Int
                ; 2. T(e2) == Int
                ; 3. T(#)  == Int
                (cases Answer (██type-of e1 TENV SUBST)
                  ($an-answer (ty1 subst1)
                              (let [(subst1 (░░unify ty1 (int-type) subst1 e1))]
                                ; ---------
                                (cases Answer (██type-of e2 TENV subst1)
                                  ($an-answer (ty2 subst2)
                                              (let [(subst2 (░░unify ty2 (int-type) subst2 e2))]
                                                ($an-answer (int-type) subst2))))))))
      (zero?-exp (e1)
                 ; T(e1) == Int
                 ; T(#)  == Bool
                 (cases Answer (██type-of e1 TENV SUBST)
                   ($an-answer (ty1 subst1)                               
                               (let [(subst2 (░░unify ty1 (int-type) subst1 EXP))]
                                 ; ---------
                                 ($an-answer (bool-type) subst2)))))
      (if-exp (e1 e2 e3)
              ; T(e1) = Bool
              ; T(#)  = T(e2)
              ; T(#)  = T(e3)
              (cases Answer (██type-of e1 TENV SUBST)
                ($an-answer (ty1 subst)
                            (let [(subst (░░unify ty1 (bool-type) subst e1))]
                              ; ---------
                              (cases Answer (██type-of e2 TENV subst)
                                ($an-answer (ty2 subst)
                                            (cases Answer (██type-of e3 TENV subst)
                                              ($an-answer (ty3 subst)
                                                          
                                                          (let [(subst (░░unify ty2 ty3 subst EXP))]
                                                            ($an-answer ty2 subst))))))))))
      ; --------------------------------------------------------------------------------                                     
      (let-exp (var e1 body)
               ; 先添加 e1 的类型，然后再得到整个 exp的类型
               (cases Answer (██type-of e1 TENV SUBST)
                 ($an-answer (ty-e1 ss-1)
                             (██type-of body ($extend-tenv var ty-e1 TENV) SUBST))))
      ;; 1-arg proc
      (proc-exp (var opt-ty body)
                (let [(var-type (opt-type->type opt-ty))]
                  (cases Answer (██type-of body ($extend-tenv var var-type TENV) SUBST)
                    ($an-answer (ty-body subst)
                                ($an-answer (proc-type var-type ty-body) subst)))))
      (call-exp (rator rand)
                (let [(result-type (fresh-tvar-type))]
                  (cases Answer (██type-of rator TENV SUBST)
                    ($an-answer (ty-rator st1)

                                (cases Answer (██type-of rand TENV st1)
                                  ($an-answer (ty-rand st2)
                                              ; T(f) = T(arg) -> T(#)
                                              (let [(st3 (░░unify ty-rator (proc-type ty-rand result-type) st2 EXP))]
                                                ($an-answer result-type st3))))))))
      ;; letrec
      (letrec-exp (p-result-otype p-name b-var b-var-otype p-body letrec-body)
                  (let [(p-result-type (opt-type->type p-result-otype))
                        (p-var-type (opt-type->type b-var-otype))]                    
                    (let [(tenv-for-letrec-body ($extend-tenv p-name  (proc-type p-var-type p-result-type) TENV))]
                      (cases Answer (██type-of p-body ($extend-tenv b-var p-var-type  tenv-for-letrec-body) SUBST)
                        ($an-answer (p-body-type subst)
                                    (let ((subst (░░unify p-body-type p-result-type subst p-body)))
                                      (██type-of letrec-body tenv-for-letrec-body subst)))))))
      ))
                  
      

  ; =============================================================  
  (define (typer src)
    (type-to-external-form (type-of-program (scan&parse src))))

  )
