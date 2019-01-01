(module unification (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  ; ███████████████ 把纸上 unification的过程 用数据结构、分解出的小api 构建出来
  ; ███████████████ 就是为type var 求出其value (Type)   【type var和 type只互相定义的】

  ;  t0 = tf → t1
  ;  t1 = tx → t2
  ;  t3 = int
  ;  t4 = int
  ;  t2 = int
  ;  tf = int → t3
  ;  tf = tx → t4
  
  ; 假设 int = 1, bool = 2,  存在变量 ti , f

  ; 【全是一次的、则7元方程需要7条不重复的等式即可】
  ; t0 = 1 -> 1 -> 1 -> 1
  ; t1 = 1 -> 1
  ; f = 1 -> 1
  ; f = x -> 1

  ;【定义基本的】
  ; x = 1 -> f
  ; f = 2
  ; ===>  x = 1 -> 2        
  
  
  ;========================================================================== substitution  (如同env: t_i <-> Type)
  (define (empty-subst)
    '())

  (define (extend-subst subst tvar ty)
    (cons (cons tvar ty)
          (map (lambda (pr) (let [(oldlhs (car pr))
                                  (oldrhs (cdr pr))]
                              (cons oldlhs (apply-one-subst oldrhs tvar ty))))
               subst)))
                              
  
  ; t0 [tv = t1]
  ; apply-one-subst :: Type * Tvar * Type -> Type

  ; e.g. (apply-one-subst  'int->t0'  t0  bool ) =  'int->bool'
  ;      (apply-one-subst (proc-type (int-type) (tvar-type 0)) (tvar-type 0) (bool-type))
  (define (apply-one-subst ty-target tvar ty1)
    (cases type ty-target
      (int-type ()
                (int-type))
      (bool-type ()
                 (bool-type))
      (proc-type (arg-type result-type)
                 (proc-type
                  (apply-one-subst arg-type tvar ty1)
                  (apply-one-subst result-type tvar ty1)))
      (tvar-type (sn)
                 (if (equal? ty-target tvar)
                     ty1
                     ty-target))
      ))

  ; substitution :  list of pairs < TVar ~ Type >
  (define (apply-subst-to-type ty subst)
    (cases type ty
      (int-type ()
                (int-type))
      (bool-type ()
                 (bool-type))
      (proc-type (t1 t2)
                 (proc-type (apply-subst-to-type t1 subst)
                            (apply-subst-to-type t2 subst)))
      (tvar-type (sn)
                 (let ((tmp (assoc ty subst)))
                   (if tmp
                       (cdr tmp)
                       ty)))
      ))
  ;========================================================================== unifier
    ;; unifier : Type * Type * Subst * Exp -> Subst OR Fails
  ;; Page: 264
  (define unifier
    (lambda (ty1 ty2 subst exp)
      (let ((ty1 (apply-subst-to-type ty1 subst))
            (ty2 (apply-subst-to-type ty2 subst)))
        (cond
          ((equal? ty1 ty2) subst)            
          ((tvar-type? ty1)
           (if (no-occurrence? ty1 ty2)
               (extend-subst subst ty1 ty2)
               (report-no-occurrence-violation ty1 ty2 exp)))
          ((tvar-type? ty2)
           (if (no-occurrence? ty2 ty1)
               (extend-subst subst ty2 ty1)
               (report-no-occurrence-violation ty2 ty1 exp)))
          ((and (proc-type? ty1) (proc-type? ty2))
           (let ((subst (unifier
                         (proc-type->arg-type ty1)
                         (proc-type->arg-type ty2)
                         subst exp)))
             (let ((subst (unifier
                           (proc-type->result-type ty1)
                           (proc-type->result-type ty2)
                           subst exp)))
               subst)))
          (else (report-unification-failure ty1 ty2 exp))))))

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
  ;; Page: 265
  (define no-occurrence?
    (lambda (tvar ty)
      (cases type ty
        (int-type () #t)
        (bool-type () #t)
        (proc-type (arg-type result-type)
                   (and
                    (no-occurrence? tvar arg-type)
                    (no-occurrence? tvar result-type)))
        (tvar-type (serial-number) (not (equal? tvar ty))))))
  



  )
