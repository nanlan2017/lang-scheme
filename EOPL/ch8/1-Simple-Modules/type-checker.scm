(module type-checker (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "lang-type.scm")
  (require "data-structures.scm")
  ; ===============================================================================================================
  ; █████████ type 也是 expression的 'val'      |        eval: expression的值是对其 sub-expression的值进行一些计算
  ;                                 对于  -(x,3) ，即 diff-exp(x , 3)
  ;     type :   int                                value:  value(x) - value(3)
  
  ; 也存在着 type rules 、type的运算
  ;                                    type-of (then..) == type-of (else..) <=========>  
  ;  对个别表达式的 type-val 有限制    type-of (diff-exp ...) = Int         <=========>   value-of (diff-exp...) = 3   
  
  ; █████████ type-check 的逻辑：
  ;             首先能够计算想关注的特定places 的expressions的 type，
  ;             然后check : 某些types equals !
      

  ; █████████ type-infer 的逻辑：
  ;             对未标记的exp/var加以 tvar， 并根据equations得到unification，从而确定types

  ; ===============================================================================================================

  
  ; lookup-module-name-in-tenv :: TEnv * Symbol -> ModuleInterface
  (define (lookup-module-name-in-tenv tenv m-name)
    (cases TEnv tenv
      ($extend-tenv-with-module (mod-id face saved-tenv)
                                (if (eqv? m-name mod-id)
                                    face
                                    (lookup-module-name-in-tenv saved-tenv m-name)))
      (else (lookup-module-name-in-tenv (get-nested-tenv tenv) m-name))))

  ; lookup-qualified-var-in-tenv :: Symbol * Symbol * TEnv -> Type                      
  (define (lookup-qualified-var-in-tenv m-name var-name tenv)
    (let ((iface (lookup-module-name-in-tenv tenv m-name)))
      (cases SimpleInterface iface
        ($a-simple-interface (decls)
                             (lookup-variable-name-in-decls var-name decls)))))

  ; lookup-variable-name-in-decls :: Symbol * [VarDeclartion] -> Type
  (define (lookup-variable-name-in-decls var-name decls)
    (cases VarDeclaration (car decls)
      ($a-var-declaration (var ty)
                          (if (eqv? var var-name)
                              ty
                              (lookup-variable-name-in-decls var-name (cdr decls))))))

  (define (add-module-defns-to-tenv defns tenv)
    (if (null? defns)
        tenv
        (cases ModuleDefinition (car defns)
          ($a-module-definition (m-name expected-iface m-body)
                                (let ((actual-iface (interface-of m-body tenv)))
                                  (if (<:-iface actual-iface expected-iface tenv)
                                      (let ((new-tenv ($extend-tenv-with-module m-name
                                                                                expected-iface
                                                                                tenv)))
                                        (add-module-defns-to-tenv (cdr defns) new-tenv))
                                      (report-module-doesnt-satisfy-iface m-name expected-iface actual-iface)))))))

  ; defns-to-decls : Listof(Defn) × Tenv → Listof(Decl)
  (define (defns-to-decls defns tenv)
    (if (null? defns)
        '()
        (cases VarDefinition (car defns)
          ($a-var-definition (var-name exp)
                             (let ((ty (type-of exp tenv)))
                               (cons ($a-var-declaration var-name ty)
                                     (defns-to-decls (cdr defns) ($extend-tenv var-name ty tenv))))))))
  
  ; interface-of :: ModuleBody * TEnv -> ModuleInterface
  (define (interface-of m-body tenv)
    (cases ModuleBody m-body
      ($a-module-body (var-defn-s)
                      ($a-simple-interface (defns-to-decls var-defn-s tenv)))))

  (define (<:-iface iface1 iface2 tenv)
    (cases SimpleInterface iface1
      ($a-simple-interface (var-decl-s1)
                           (cases SimpleInterface iface2
                             ($a-simple-interface (var-decl-s2)
                                                  (<:-decls var-decl-s1 var-decl-s2 tenv))))))

  (define (decl->name decl)
    (cases VarDeclaration decl
      ($a-var-declaration (var ty)
                          var)))
  
  (define (decl->type decl)
    (cases VarDeclaration decl
      ($a-var-declaration (var ty)
                          ty)))

  ; [ a:Int ; b:Int]   <:   [a:Int]
  (define (<:-decls decls1 decls2 tenv)
    (cond
      ((null? decls2) #t)
      ((null? decls1) #f)
      (else (let ((name1 (decl->name (car decls1)))
                  (name2 (decl->name (car decls2))))
              (if (eqv? name1 name2)
                  (and (equal? (decl->type (car decls1)) (decl->type (car decls2)))
                       (<:-decls (cdr decls1) (cdr decls2) tenv))
                  (<:-decls (cdr decls1) decls2 tenv))))))
    
  
  (define (report-module-doesnt-satisfy-iface m-name expected-type actual-type)
    (eopl:printf  (list 'error-in-defn-of-module: m-name 'expected-type: expected-type 'actual-type: actual-type))
    (eopl:error 'type-of-module-defn))
  ; ===============================================================================================================
  ;; check-equal-type! : Type * Type * Exp -> Unspecified
  (define check-equal-type!
    (lambda (ty1 ty2 exp)
      (when (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp))))

  ;; report-unequal-types : Type * Type * Exp -> Unspecified
  (define report-unequal-types
    (lambda (ty1 ty2 exp)
      (eopl:error 'check-equal-type!  "Types didn't match: ~s != ~a in~%~a"  (type-to-external-form ty1) (type-to-external-form ty2) exp)))
  
  (define (type-of-program pgm)
    (cases Program pgm
      ($a-program (mod-defn-s exp)
                  (type-of exp (add-module-defns-to-tenv mod-defn-s (init-tenv))))))

  (define (type-of exp tenv)
    (cases Expression exp
      ($const-exp (num) (int-type))
        
      ($diff-exp (exp1 exp2)
                (let ((type1 (type-of exp1 tenv))
                      (type2 (type-of exp2 tenv)))
                  (check-equal-type! type1 (int-type) exp1)
                  (check-equal-type! type2 (int-type) exp2)
                  (int-type)))
        
      ($zero?-exp (exp1)
                 (let ((type1 (type-of exp1 tenv)))
                   (check-equal-type! type1 (int-type) exp1)
                   (bool-type)))
        
      ($if-exp (exp1 exp2 exp3)
              (let ((ty1 (type-of exp1 tenv))
                    (ty2 (type-of exp2 tenv))
                    (ty3 (type-of exp3 tenv)))
                (check-equal-type! ty1 (bool-type) exp1)
                (check-equal-type! ty2 ty3 exp)
                ty2))
        
      ($var-exp (var) (apply-tenv tenv var))

      ;; lookup-qualified-var-in-tenv defined on page 285.
      ($qualified-var-exp (m-name var-name) 
                         (lookup-qualified-var-in-tenv m-name var-name tenv))

      ($let-exp (var exp1 body)
               (let ((rhs-type (type-of exp1 tenv)))
                 (type-of body (extend-tenv var rhs-type tenv))))
        
      ($proc-exp (bvar bvar-type body)
                (let ((expanded-bvar-type
                       (expand-type bvar-type tenv)))
                  (let ((result-type
                         (type-of body
                                  (extend-tenv
                                   bvar
                                   expanded-bvar-type
                                   tenv))))
                    (proc-type expanded-bvar-type result-type))))
        
      ($call-exp (rator rand) 
                (let ((rator-type (type-of rator tenv))
                      (rand-type  (type-of rand tenv)))
                  (cases type rator-type
                    (proc-type (arg-type result-type)
                               (begin
                                 (check-equal-type! arg-type rand-type rand)
                                 result-type))
                    (else
                      (eopl:error 'type-of
                                  "Rator not a proc type:~%~s~%had rator type ~s"   
                                  rator (type-to-external-form rator-type))))))
        
      ($letrec-exp (proc-result-type proc-name 
                                    bvar bvar-type 
                                    proc-body
                                    letrec-body)
                  (let ((tenv-for-letrec-body
                         (extend-tenv 
                          proc-name
                          (expand-type
                           (proc-type bvar-type proc-result-type)
                           tenv)
                          tenv)))
                    (let ((proc-result-type
                           (expand-type proc-result-type tenv))
                          (proc-body-type
                           (type-of proc-body
                                    (extend-tenv
                                     bvar
                                     (expand-type bvar-type tenv)
                                     tenv-for-letrec-body))))
                      (check-equal-type!
                       proc-body-type proc-result-type proc-body)
                      (type-of letrec-body tenv-for-letrec-body))))
        
      ))

   

  )
