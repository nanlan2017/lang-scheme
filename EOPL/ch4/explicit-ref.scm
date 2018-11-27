(module explicit-ref (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))
  
  ;;▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ lang.scm
  
  ; the-lexical-spec
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      ; identifier
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      ; number
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  ; the-grammar
  (define the-grammar
    '(
      ; program
      (program (expression)
               a-program)

      ; expression
      (expression (number)
                  const-exp)
      
      (expression
       ("-" "(" expression "," expression ")")
       diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier)
                  var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      (expression
       ("letrec"
        (arbno identifier "(" identifier ")" "=" expression)
        "in" expression)
       letrec-exp)
      
      ;; new for explicit-refs
      (expression
       ("begin" expression (arbno ";" expression) "end")
       begin-exp)

      (expression
       ("newref" "(" expression ")")
       newref-exp)

      (expression
       ("deref" "(" expression ")")
       deref-exp)

      ;; setref
      (expression
       ("setref" "(" expression "," expression ")")
       setref-exp)

      ))
  
  ; sllgen boilerplate  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  ;;▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ store.scm
  ;; 用一个List来模仿 storage存储区
  ;; 把Ref变量当成该store的索引
  (define instrument-newref (make-parameter #f))
  
  ;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;
  
  ;;; world's dumbest model of the store:  the store is a list and a
  ;;; reference is number which denotes a position in the list.

  ;; the-store: a Scheme variable containing the current state of the
  ;; store.  Initially set to a dummy variable.
  (define the-store 'uninitialized)

  ;; empty-store : () -> Sto
  ;; Page: 111
  (define empty-store
    (lambda () '()))
  
  ;; initialize-store! : () -> Sto
  ;; usage: (initialize-store!) sets the-store to the empty-store
  ;; Page 111
  (define initialize-store!
    (lambda ()
      (set! the-store (empty-store))))

  ;; get-store : () -> Sto
  ;; Page: 111
  ;; This is obsolete.  Replaced by get-store-as-list below
  (define get-store
    (lambda () the-store))

  ;; reference? : SchemeVal -> Bool
  ;; Page: 111
  (define reference?
    (lambda (v)
      (integer? v)))

  ;; newref : ExpVal -> Ref
  ;; Page: 111
  (define newref
    (lambda (val)
      (let ((next-ref (length the-store)))
        (set! the-store
              (append the-store (list val)))
        (when (instrument-newref)
          (eopl:printf 
           "newref: allocating location ~s with initial contents ~s~%"
           next-ref val))                     
        next-ref)))                     

  ;; deref : Ref -> ExpVal
  ;; Page 111
  (define deref 
    (lambda (ref)
      (list-ref the-store ref)))

  ;; setref! : Ref * ExpVal -> Unspecified
  ;; Page: 112
  (define setref!                       
    (lambda (ref val)
      (set! the-store
            (letrec
                ((setref-inner
                  ;; returns a list like store1, except that position ref1
                  ;; contains val. 
                  (lambda (store1 ref1)
                    (cond
                      ((null? store1)
                       (report-invalid-reference ref the-store))
                      ((zero? ref1)
                       (cons val (cdr store1)))
                      (else
                        (cons
                         (car store1)
                         (setref-inner
                          (cdr store1) (- ref1 1))))))))
              (setref-inner the-store ref)))))

  (define report-invalid-reference
    (lambda (ref the-store)
      (eopl:error 'setref
                  "illegal reference ~s in store ~s"
                  ref the-store)))

  ;; get-store-as-list : () -> Listof(List(Ref,Expval))
  ;; Exports the current state of the store as a scheme list.
  ;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
  ;;   where foo, bar, and baz are expvals.
  ;; If the store were represented in a different way, this would be
  ;; replaced by something cleverer.
  ;; Replaces get-store (p. 111)
  (define get-store-as-list
    (lambda ()
      (letrec
          ((inner-loop
            ;; convert sto to list as if its car was location n
            (lambda (sto n)
              (if (null? sto)
                  '()
                  (cons
                   (list n (car sto))
                   (inner-loop (cdr sto) (+ n 1)))))))
        (inner-loop the-store 0))))
  ;;▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ data-structures.scm
  
  ;---------------------------------------------------------------------------------- ExpVal
  ; data ExpVal = NumVal Number | BoolVal bool | ProcVal Proc | RefVal Reference
  (define-datatype expval expval?
    (num-val
     (value number?))
    (bool-val
     (boolean boolean?))
    (proc-val 
     (proc proc?))
    (ref-val
     (ref reference?))
    )
  
  (define expval->num
    (lambda (v)
      (cases expval v
        (num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
        (bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
        (proc-val (proc) proc)
        (else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
        (ref-val (ref) ref)
        (else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))

  ;---------------------------------------------------------------------------------
  ;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
     (bvar symbol?)
     (body expression?)
     (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
     (bvar symbol?)
     (bval expval?)
     (saved-env environment?))
    (extend-env-rec*
     (proc-names (list-of symbol?))
     (b-vars (list-of symbol?))
     (proc-bodies (list-of expression?))
     (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
        (empty-env () '())
        (extend-env (sym val saved-env)
                    (cons
                     (list sym (expval->printable val))
                     (env->list saved-env)))
        (extend-env-rec* (p-names b-vars p-bodies saved-env)
                         (cons
                          (list 'letrec p-names '...)
                          (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
        (proc-val (p)
                  (cases proc p
                    (procedure (var body saved-env)
                               (list 'procedure var '... (env->list saved-env)))))
        (else val))))

  ;;▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ environments.scm
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69
  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

  ;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
                   (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (bvar bval saved-env)
                    (if (eqv? search-sym bvar)
                        bval
                        (apply-env saved-env search-sym)))
        (extend-env-rec* (p-names b-vars p-bodies saved-env)
                         (cond 
                           ((location search-sym p-names)
                            => (lambda (n)
                                 (proc-val
                                  (procedure 
                                   (list-ref b-vars n)
                                   (list-ref p-bodies n)
                                   env))))
                           (else (apply-env saved-env search-sym)))))))

  ;; location : Sym * Listof(Sym) -> Maybe(Int)
  ;; (location sym syms) returns the location of sym in syms or #f is
  ;; sym is not in syms.  We can specify this as follows:
  ;; if (memv sym syms)
  ;;   then (list-ref syms (location sym syms)) = sym
  ;;   else (location sym syms) = #f
  (define location
    (lambda (sym syms)
      (cond
        ((null? syms) #f)
        ((eqv? sym (car syms)) 0)
        ((location sym (cdr syms))
         => (lambda (n) 
              (+ n 1)))
        (else #f))))

  ;;▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ interp.scm

 
  ;; value-of : Exp * Env -> ExpVal
  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (num-val
                       (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((num1 (expval->num val1)))
                       (if (zero? num1)
                           (bool-val #t)
                           (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (if (expval->bool val1)
                      (value-of exp2 env)
                      (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
                 (let ((val1 (value-of exp1 env)))
                   (value-of body
                             (extend-env var val1 env))))
        
        (proc-exp (var body)
                  (proc-val (procedure var body env)))

        (call-exp (rator rand)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg (value-of rand env)))
                    (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
                    (value-of letrec-body
                              (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
                   (letrec 
                       ((value-of-begins
                         (lambda (e1 es)
                           (let ((v1 (value-of e1 env)))
                             (if (null? es)
                                 v1
                                 (value-of-begins (car es) (cdr es)))))))
                     (value-of-begins exp1 exps)))

        (newref-exp (exp1)
                    (let ((v1 (value-of exp1 env)))
                      (ref-val (newref v1))))

        (deref-exp (exp1)
                   (let ((v1 (value-of exp1 env)))
                     (let ((ref1 (expval->ref v1)))
                       (deref ref1))))

        (setref-exp (exp1 exp2)
                    (let ((ref (expval->ref (value-of exp1 env))))
                      (let ((v2 (value-of exp2 env)))
                        (begin
                          (setref! ref v2)
                          (num-val 23)))))
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
                   (let ((r arg))
                     (let ((new-env (extend-env var r saved-env)))
                       (when (instrument-let)
                         (begin
                           (eopl:printf
                            "entering body of proc ~s with env =~%"
                            var)
                           (pretty-print (env->list new-env))
                           (eopl:printf "store =~%")
                           (pretty-print (store->readable (get-store-as-list)))
                           (eopl:printf "~%")))
                       (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
       (lambda (p)
         (cons
          (car p)
          (expval->printable (cadr p))))
       l)))


  ;;▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ tests.scm
  ;; code-text-N
  (define simple-store-test-1 '("let x = newref(17) in deref(x)" 17))

  ;;▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ top.scm
  ;; run : String -> ExpVal
  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))


  )











































































