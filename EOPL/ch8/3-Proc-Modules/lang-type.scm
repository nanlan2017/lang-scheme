(module lang-type (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  ; ===============================================================


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

        
  )
