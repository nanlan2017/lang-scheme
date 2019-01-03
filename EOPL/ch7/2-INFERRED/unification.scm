(module unification (lib "eopl.ss" "eopl")
  (require "lang.scm")
  (require "lang-type.scm")

  (provide substitution?
           empty-subst

           apply-substs-to-type
           ░░unify)
  ;=========================================================================
  ; Briefly, unification is the process of finding a substitution that makes two given terms equal.
  ; type inference is done by applying unification to type expressions (e.g. 'a -> 'b -> 'a) 
  
  ; ███████████████ 把纸上 unification的过程 用数据结构、分解出的小api 构建出来
  ; ███████████████ 就是为type var 求出其value (Type)   【type var和 type只互相定义的】

  
  
  ;========================================================================== substitution  (如同env: t_i <-> Type)
  (define (empty-subst)
    '())

  ; ███████ extend-subst :: 6 * tv * t -> 6[tv=t]   // 6[..] 仍然是一个被加入equal pairs的 substitutions
  (define (extend-subst subst tvar ty)
    (cons (cons tvar ty)
          (map (lambda (pr) (let [(oldlhs (car pr))
                                  (oldrhs (cdr pr))]
                              (cons oldlhs (apply-1-subst oldrhs tvar ty))))
               subst)))

  ; 辅助 @ substitution?
  (define pair-of
    (lambda (pred1 pred2)
      (lambda (val)
        (and (pair? val) (pred1 (car val)) (pred2 (cdr val))))))

  (define substitution? 
    (list-of (pair-of tvar-type? type?)))

  ; 辅助 @ extend-subst
  ; apply-one-subst :: Type * Tvar * Type -> Type
  ; 例子：t2 ~ bool->t3  [t3 = int]   ====>   t2 ~ bool->int ;  t3 ~ int    
  (define (apply-1-subst ty-target tvar ty1)
    (cases type ty-target
      (int-type ()
                (int-type))
      (bool-type ()
                 (bool-type))
      (proc-type (arg-type result-type)
                 (proc-type
                  (apply-1-subst arg-type tvar ty1)
                  (apply-1-subst result-type tvar ty1)))
      (tvar-type (sn)
                 (if (equal? ty-target tvar)
                     ty1
                     ty-target))
      ))

  ; ███████ t * 6 -> t6     // t6 仍然是一个被替换过的 type-exp
  (define (apply-substs-to-type ty subst)
    (cases type ty
      (int-type ()
                (int-type))
      (bool-type ()
                 (bool-type))
      (proc-type (t1 t2)
                 (proc-type (apply-substs-to-type t1 subst)
                            (apply-substs-to-type t2 subst)))
      (tvar-type (sn)
                 (let ((tmp (assoc ty subst)))
                   (if tmp
                       (cdr tmp)
                       ty)))
      ))
  ;========================================================================== unifier
  ;; unifier : Type1 * Type2 * Subst * Exp ---> Subst OR Fails
  ;; 更新subst : 使得 Type1 = Type2 这条约束equation成立。
  (define ░░unify
    (lambda (ty1 ty2 subst exp)
      (let ((ty1 (apply-substs-to-type ty1 subst))
            (ty2 (apply-substs-to-type ty2 subst)))
        (cond
          ; 逻辑过程: P 263
          ((equal? ty1 ty2)
           subst)
          ; t0 == ...
          ((tvar-type? ty1)
           (if (no-occurrence? ty1 ty2)
               (extend-subst subst ty1 ty2)
               (report-no-occurrence-violation ty1 ty2 exp)))
          ; ... == t0
          ((tvar-type? ty2)
           (if (no-occurrence? ty2 ty1)
               (extend-subst subst ty2 ty1)
               (report-no-occurrence-violation ty2 ty1 exp)))
          
          ((and (proc-type? ty1) (proc-type? ty2))
           (let ((subst (░░unify
                         (proc-type->arg-type ty1)
                         (proc-type->arg-type ty2)
                         subst exp)))
             (let ((subst (░░unify
                           (proc-type->result-type ty1)
                           (proc-type->result-type ty2)
                           subst exp)))
               subst)))
          (else (report-unification-failure ty1 ty2 exp))))))
  
  ;------------------------------------------------------------------------
  (define report-unification-failure
    (lambda (ty1 ty2 exp) 
      (eopl:error 'unification-failure
                  "Type mismatch: ~s doesn't match ~s in ~s~%"
                  (type-to-external-form ty1)
                  (type-to-external-form ty2)
                  exp)))

  (define report-no-occurrence-violation
    (lambda (ty1 ty2 exp)
      (eopl:error 'check-no-occurence!
                  "Can't unify: type variable ~s occurs in type ~s in expression ~s~%" 
                  (type-to-external-form ty1)
                  (type-to-external-form ty2)
                  exp)))

  
  ;; no-occurrence? : Tvar * Type -> Bool
  ;; usage: Is there an occurrence of tvar in ty?
  ;; 举例   t0 ~  int -> t0  就 occur了！
  (define no-occurrence?
    (lambda (tvar tyexp)
      (cases type tyexp
        (int-type () #t)
        (bool-type () #t)
        (proc-type (arg-type result-type)
                   (and
                    (no-occurrence? tvar arg-type)
                    (no-occurrence? tvar result-type)))
        (tvar-type (serial-number) (not (equal? tvar tyexp))))))
  



  )
