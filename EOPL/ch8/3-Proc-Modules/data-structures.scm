(module data-structures (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  (require "utils.scm")
  ;;============================================================= Expressed Value
  (define-datatype ExpVal ExpVal?
    ($num-val
     (num number?))
    ($bool-val
     (v boolean?))
    ($proc-val
     (p Proc?))
    )

  ;; expval -> number
  (define (expval->num expval)
    (cases ExpVal expval
      ($num-val (n) n)
      (else (eopl:error "Can't get num-val from ExpVal :" expval))
      ))
  ;; expval -> boolean
  (define (expval->bool expval)
    (cases ExpVal expval
      ($bool-val (b) b)
      (else (eopl:error "Can't get bool-val from ExpVal :" expval))
      ))
  ;; expval -> Proc
  (define (expval->proc expval)
    (cases ExpVal expval
      ($proc-val (p) p)
      (else (eopl:error "Can't get proc-val from ExpVal :" expval))
      ))
  ;;============================================================= Proc
  (define-datatype Proc Proc?
    ($procedure
     (var identifier?)
     (body Expression?)
     (env Env?)))
    
  ;;============================================================= Env : eval过程中、需保存的上下文值
  (define-datatype Env Env?
    ($empty-env)   
    ($extend-env
     (var symbol?)
     (val ExpVal?)
     (env Env?))
    ($extend-env-rec
     (p-name identifier?)
     (b-var identifier?)
     (body Expression?)
     (env Env?))
    ; module
    ($extend-env-with-module
     (mod-id symbol?)
     (mod-val TypedModule?)
     (env Env?))
    )

  (define (get-nested-env env)
    (cases Env env
      ($empty-env ()
                  (eopl:error 'get-nested-env "No nested env for Empty-env!"))
      ($extend-env (saved-var saved-val saved-env)
                   saved-env)
      ($extend-env-rec (p-name b-var p-body saved-env)
                       saved-env)
      ($extend-env-with-module (mod-id mod-val saved-env)
                               saved-env)
      ))

  (define (init-env)
    ($extend-env 'i ($num-val 1)
                 ($extend-env 'v ($num-val 5)
                              ($extend-env 'x ($num-val 10) ($empty-env)))))

  ; extend-env* :: [symbol] x [ExpVal] x Env -> Env
  (define (extend-env* vars expvals env)
    (if (null? vars)
        env
        (let [(new-env ($extend-env (car vars) (car expvals) env))]
          (extend-env* (cdr vars) (cdr expvals) new-env))))

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
      ($extend-env-with-module (mod-id mod-val saved-env)
                               (apply-env saved-env var))
      ))
  ;----------------------------------------------------------- Observer: 在Env中查找module内的var的值
  (define-datatype TypedModule TypedModule?
    ($a-simple-module
     (bindings Env?))
    ;
    ($proc-module           ; 对应于Proc (其实就是保存Proc定义体)
     (b-var symbol?)
     (body ModuleBody?)
     (env Env?))
    )

  ; -> TypedModule
  (define (lookup-module-in-env mod-name env)
    (cases Env env
      ($extend-env-with-module (mod-id mod-val saved-env)
                               (if (eqv? mod-name mod-id)
                                   mod-val
                                   (lookup-module-in-env mod-name saved-env)))
      (else (lookup-module-in-env mod-name (get-nested-env env)))))

  (define (lookup-qualified-var-in-env mod-name var-name env)
    (let [(mod-val (lookup-module-in-env mod-name env))]
      (cases TypedModule mod-val
        ($a-simple-module (bindings)
                          (apply-env bindings var-name)))))
  ;;============================================================== TEnv : typer过程中，需保存的上下文类型
  (define-datatype TEnv TEnv?
    ($empty-tenv)
    ($extend-tenv
     (var symbol?)
     (ty Type?)
     (tenv TEnv?))
    ; module
    ($extend-tenv-with-module
     (name symbol?)
     (face SimpleInterface?)
     (tenv TEnv?))
    ; expanded-type
    ($extend-tenv-with-type
     (name Type?)  ; named type:  ① transparent Point = Int  ② type Point = Int    |    qualified type(opaque type) :  m1::Point
     (type Type?)  ;         m1::Point ~  Int                                      |           m1::Point ~ m1::Point
     (tenv TEnv?))
    )

  (define (get-nested-tenv tenv)
    (cases TEnv tenv
      ($empty-tenv ()
                   (eopl:error 'get-nested-tenv "No nested tenv for Empty-tenv !"))
      ($extend-tenv (saved-var saved-ty saved-tenv)
                    saved-tenv)
      ($extend-tenv-with-module (mod-name face saved-tenv)
                                saved-tenv)
      ($extend-tenv-with-type (ty etype saved-tenv)
                              saved-tenv)
      ))

  (define (init-tenv)
    ($extend-tenv 'i ($int-type)
                  ($extend-tenv 'v ($int-type)
                                ($extend-tenv 'x ($int-type) ($empty-tenv)))))
  
  (define (apply-tenv tenv var)
    (cases TEnv tenv
      ($empty-tenv ()
                   (eopl:error 'apply-tenv "Didn't find in type-env while search : ~s" var))
      ($extend-tenv (saved-var saved-ty saved-tenv)
                    (if (equal? var saved-var)
                        saved-ty
                        (apply-tenv saved-tenv var)))
      ($extend-tenv-with-module (mod-name face saved-tenv)
                                (apply-tenv saved-tenv var))
      ($extend-tenv-with-type (ty etype saved-tenv)
                              (apply-tenv saved-tenv var))

      ))
  ;----------------------------------------------------------- Observer: 在TEnv中查找module内的var的type
  ; lookup-module-name-in-tenv :: TEnv * Symbol -> ModuleInterface
  (define (lookup-module-in-tenv tenv m-name)
    (cases TEnv tenv
      ($extend-tenv-with-module (mod-id face saved-tenv)
                                (if (eqv? m-name mod-id)
                                    face
                                    (lookup-module-in-tenv saved-tenv m-name)))
      (else (lookup-module-in-tenv (get-nested-tenv tenv) m-name))))

  ; lookup-qualified-var-in-tenv :: Symbol * Symbol * TEnv -> Type                      
  (define (lookup-qualified-var-in-tenv m-name var-name tenv)
    (let ((iface (lookup-module-in-tenv tenv m-name)))
      (cases SimpleInterface iface
        ($a-simple-interface (decls)
                             (lookup-variable-name-in-decls var-name decls)))))

  ; lookup-variable-name-in-decls :: Symbol * [VarDeclaration] -> Type
  (define (lookup-variable-name-in-decls var-name decls)
    (cases VarDeclaration (car decls)
      ($a-var-declaration (var ty)
                          (if (eqv? var var-name)
                              ty
                              (lookup-variable-name-in-decls var-name (cdr decls))))
      (else (lookup-variable-name-in-decls var-name (cdr decls)))))
                         
     

  )
