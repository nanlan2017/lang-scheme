(module type-checker (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  ; =========================================================================
  ; check-equal-type! :: Type * Type * Exp -> ()
  (define (★check-equal-type! ty1 ty2 exp)
    (if (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp)
        #t))

  (define (report-unequal-types ty1 ty2 exp)
    (eopl:error 'check-equal-type! "Types didn’t match: ~s != ~a in~%~a"
                (type-to-external-form ty1) (type-to-external-form ty2) exp))

  (define (report-rator-not-a-proc-type rator-type rator)
    (eopl:error 'report-rator-not-a-proc-type "Actual Rator type is ~s in ~s , that's illegal!~n" rator-type rator))

  (define (type-to-external-form ty)
    (cases type ty
      (int-type ()
                'int)
      (bool-type ()
                 'bool)
      (proc-type (arg-ty res-ty)
                 (list (type-to-external-form arg-ty) '-> (type-to-external-form res-ty)))
      ))
  ; =======================================================================
  (define (typeof-program prog)
    (cases program prog
      (a-program (expr)
                 (typeof expr (init-tyenv)))))

  ; typeof :: Exp * TyEnv -> type
  (define (typeof exp tyenv)
    (cases expression exp
      (const-exp (n)
                 (int-type))
      (var-exp (x)
               (apply-tyenv tyenv x))
      (diff-exp (e1 e2)
                (let [(ty1 (typeof e1 tyenv))
                      (ty2 (typeof e2 tyenv))]
                  (★check-equal-type! ty1 (int-type) e1)
                  (★check-equal-type! ty2 (int-type) e1)
                  (int-type)))
      (zero?-exp (e1)
                 (let [(ty1 (typeof e1 tyenv))]
                   (★check-equal-type! ty1 (int-type) e1)
                   (bool-type)))
      (if-exp (e1 e2 e3)
              (let ((ty1 (typeof e1 tyenv))
                    (ty2 (typeof e2 tyenv))
                    (ty3 (typeof e3 tyenv)))
                (★check-equal-type! ty1 (bool-type) e1)
                (★check-equal-type! ty2 ty3 exp)
                ty2))
      (let-exp (var e1 body)
               (let [(ty1 (typeof e1 tyenv))]
                 (typeof body ($extend-tyenv var ty1 tyenv))))
      ;; 1-arg proc
      (proc-exp (var ty body)
                (let [(res-type (typeof body ($extend-tyenv var ty tyenv)))]
                  (proc-type ty res-type)))
      (call-exp (rator rand)
                (let ((rator-type (typeof rator tyenv))
                      (rand-type (typeof rand tyenv)))
                  (cases type rator-type
                    (proc-type (arg-type result-type)
                               (begin
                                 (★check-equal-type! arg-type rand-type rand)
                                 result-type))
                    (else (report-rator-not-a-proc-type rator-type rator)))))                               
      ;; letrec
      (letrec-exp (p-res-type pid b-var b-var-type p-body letrec-body)
                  (let* [(new-tyenv ($extend-tyenv pid (proc-type b-var-type p-res-type) tyenv))
                         (p-body-type (typeof p-body ($extend-tyenv b-var b-var-type new-tyenv)))]
                    (★check-equal-type! p-res-type p-body-type p-body)
                    (typeof letrec-body new-tyenv)))
                  
      ))

  ; =============================================================  
  (define (check src)
    (type-to-external-form(typeof-program (scan&parse src))))

  )
