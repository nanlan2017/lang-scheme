(module lang-type (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "0-lang.scm")
  ;;============================================================== TEnv
  ;  we introduce a systematic way of handling opaque and transparent types.
  ;  An opaque type behaves like a primitive type, such as int or bool.
  ;  Transparent types, on the other hand, are transparent, as the name suggests: they behave exactly like their definitions.
  ;  ████ So every type is equivalent to one that is given by the grammar
  ;             Type ::= int | bool | (Type -> Type) | from m take t   【type alias不是Type的“运行时”值,而像一个Type Ref而已】
  ;  where t is declared as an opaque type in m. We call a type of this form an expanded type.
  ;  We next extend type environments to handle new types. Our type environments will bind each named type or qualified type to an expanded type.
  
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
    ($extend-tenv-with-type  ; 对应body里的 $type-definition
     (name Type?)   ; 'literal'
     (type Type?)   ; ★ expanded-type
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
    ($empty-tenv))
  
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
  (define (lookup-tenv/id->iface tenv m-name)
    (cases TEnv tenv
      ($extend-tenv-with-module (mod-id face saved-tenv)
                                (if (eqv? m-name mod-id)
                                    face
                                    (lookup-tenv/id->iface saved-tenv m-name)))
      (else (lookup-tenv/id->iface (get-nested-tenv tenv) m-name))))

  ; lookup-qualified-var-in-tenv :: Symbol * Symbol * TEnv -> Type                      
  (define (lookup-tenv/qualified-var=>type m-name var-name tenv)
    (let ((iface (lookup-tenv/id->iface tenv m-name)))
      (cases SimpleInterface iface
        ($a-simple-interface (decls)
                             (lookup-decls/var=>type var-name decls)))))

  ; lookup-variable-name-in-decls :: Symbol * [VarDeclaration] -> Type
  (define (lookup-decls/var=>type var-name decls)
    (cases VarDeclaration (car decls)
      ($a-var-declaration (var ty)
                          (if (eqv? var var-name)
                              ty
                              (lookup-decls/var=>type var-name (cdr decls))))
      (else (lookup-decls/var=>type var-name (cdr decls)))))  
  ; =============================================================================
  ;; check-equal-type! : Type * Type * Exp -> Unspecified
  (define check-equal-type!
    (lambda (ty1 ty2 exp)
      (when (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp))))

  ;; report-unequal-types : Type * Type * Exp -> Unspecified
  (define report-unequal-types
    (lambda (ty1 ty2 exp)
      (eopl:error 'check-equal-type!  "Types didn't match: ~s != ~a in~%~a"  (type-to-external-form ty1) (type-to-external-form ty2) exp)))
  
  ;; type-to-external-form : Type -> List
  (define type-to-external-form
    (lambda (ty)
      (cases Type ty
        ($int-type ()
                   'int)
        ($bool-type ()
                    'bool)
        ($proc-type (arg-type result-type)
                    (list (type-to-external-form arg-type) '-> (type-to-external-form result-type)))
        ;
        ($named-type (t)
                     t)
        ($qualified-type (mod-name ty)
                         (string->symbol (string-append (symbol->string mod-name) "::"(symbol->string ty))))
        )))
  ; ===========================================================================
  (define (decl->name decl)
    (cases VarDeclaration decl
      ($a-var-declaration (var ty)
                          var)
      (else #f)
      ))
  
  (define (decl->type decl)
    (cases VarDeclaration decl
      ($a-var-declaration (var ty)
                          ty)
      (else #f)))      
  )
