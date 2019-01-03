(module lang-type (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")

  ; ===============================================================
  ;  (define-datatype type type?
  ;    (int-type)
  ;    (bool-type)
  ;
  ;    (proc-type
  ;     (param-ty type?)
  ;     (res-ty type?))
  ;    (tvar-type
  ;     (sn number?))
  ;    )
  ;  
  ;  (define-datatype optional-type optional-type?
  ;    (no-type)
  ;    (a-type
  ;     (ty type?))
  ;    )
  ; -------------------------------------------------------------------

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
        )))

        
  )
