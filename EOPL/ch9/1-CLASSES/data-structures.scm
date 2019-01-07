(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?
  (require "classes.scm")               ; for object?

  (provide (all-defined-out))

  ; =================================================  expressed values
  (define-datatype ExpVal ExpVal?
    ($num-val
     (value number?))
    ($bool-val
     (boolean boolean?))
    ($proc-val 
     (proc Proc?))
    ($ref-val
     (ref reference?))
    ($obj-val
     (obj Object?))
    ($list-val
     (lst (list-of ExpVal?)))
    )

  ; extractors:

  (define expval->num
    (lambda (v)
      (cases ExpVal v
        ($num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases ExpVal v
        ($bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases ExpVal v
        ($proc-val (proc) proc)
        (else (expval-extractor-error 'proc v)))))

  ;; not used.  Nor is expval->obj or expval->list, so we haven't written them.
  (define expval->ref
    (lambda (v)
      (cases ExpVal v
        ($ref-val (ref) ref)
        (else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))

  ; =================================================== procedures

  (define-datatype Proc Proc?
    ($procedure
     (vars (list-of symbol?))
     (body Expression?)
     (env Env?)))
  
  (define-datatype Env Env?
    ($empty-env)
    ($extend-env 
     (bvars (list-of symbol?))
     (bvals (list-of reference?)) 
     (saved-env Env?))
    ($extend-env-rec**
     (proc-names (list-of symbol?))
     (b-varss (list-of (list-of symbol?)))
     (proc-bodies (list-of Expression?))
     (saved-env Env?))
    ($extend-env-with-self-and-super  ; binds :  %self ~~ an object  |  %super ~~ a class name
     (self Object?)
     (super-name symbol?)
     (saved-env Env?))
    )

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases Env env
        ($empty-env () '())
        ($extend-env (sym val saved-env)
                     (cons
                      (list sym val)
                      (env->list saved-env)))
        ($extend-env-rec** (p-names b-varss p-bodies saved-env)
                           (cons
                            (list 'letrec p-names '...)
                            (env->list saved-env)))
        ($extend-env-with-self-and-super (self super-name saved-env)
                                         (cons
                                          (list 'self self 'super super-name)
                                          (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned up with env->list 
  (define expval->printable
    (lambda (val)
      (cases ExpVal val
        ($proc-val (p)
                   (cases Proc p
                     ($procedure (var body saved-env)
                                 (list 'procedure var '... (env->list saved-env)))))
        (else val))))

  )
