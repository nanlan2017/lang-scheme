(module lang-type-helpers (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  ;;=========================================================== tenv (symbol <-> type)
  (define-datatype TEnv TEnv?
    ($empty-tenv)
    ($extend-tenv
     (var symbol?)
     (ty type?)
     (tyenv TEnv?))
    )

  (define (init-tenv)
    ($extend-tenv 'i (int-type)
                   ($extend-tenv 'v (int-type)
                                  ($extend-tenv 'x (int-type) ($empty-tenv)))))
  (define (apply-tenv tenv var)
    (cases TEnv tenv
      ($empty-tenv ()
                    (eopl:error 'apply-tyenv "Didn't find in type-env while search : ~s" var))
      ($extend-tenv (saved-var saved-ty saved-tenv)
                     (if (equal? var saved-var)
                         saved-ty
                         (apply-tenv saved-tenv var)))))
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
  
  ; ===============================================================
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

  (define fresh-tvar-type
    (let [(sn 0)]
      (lambda ()
        (set! sn (+ 1 sn))
        (tvar-type sn))))

  
  (define (opt-type->type otype)
    (cases optional-type otype
      (no-type ()
               (fresh-tvar-type))
      (a-type (ty)
              ty)
      ))

        
  )
