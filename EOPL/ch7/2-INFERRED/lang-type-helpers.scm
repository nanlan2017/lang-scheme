(module lang-type-helpers (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  ; ===============================================================
  (define atomic-type?
    (lambda (ty)
      (cases type ty
        (proc-type (ty1 ty2) #f)
        (tvar-type (sn) #f)
        (else #t))))

  (define proc-type?
    (lambda (ty)
      (cases type ty
        (proc-type (t1 t2) #t)
        (else #f))))

  (define tvar-type?
    (lambda (ty)
      (cases type ty
        (tvar-type (serial-number) #t)
        (else #f))))

  (define proc-type->arg-type
    (lambda (ty)
      (cases type ty
        (proc-type (arg-type result-type) arg-type)
        (else (eopl:error 'proc-type->arg-type "Not a proc type: ~s" ty)))))

  (define proc-type->result-type
    (lambda (ty)
      (cases type ty
        (proc-type (arg-type result-type) result-type)
        (else (eopl:error 'proc-type->result-types  "Not a proc type: ~s" ty)))))

  ;; type-to-external-form : Type -> List
  (define type-to-external-form
    (lambda (ty)
      (cases type ty
        (int-type () 'int)
        (bool-type () 'bool)
        (proc-type (arg-type result-type)
          (list (type-to-external-form arg-type) '-> (type-to-external-form result-type)))
        (tvar-type (serial-number)
          (string->symbol (string-append "tvar" (number->string serial-number))))
        )))

  )
