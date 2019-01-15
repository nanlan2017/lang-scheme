(module interp (lib "eopl.ss" "eopl")
  
  (require "0-lang.scm")
  (require "0-debug.scm")
  (require "0-store.scm")
  (require "1-data-structures.scm")
  (require "1-data-structures-continuation.scm")    
  (require "2-scheduler.scm")
  (require "2-semaphores.scm")     
  
  (provide (all-defined-out))

  (define (apply-procedure/k f argv cont)
    (cases Proc f
      ($procedure (var body saved-env)
                  (eval/k body ($extend-env var (newref argv) saved-env) cont))))

  (define (apply-unop/k unop1 val cont)
    (cases unop unop1
      (zero?-unop () (apply-cont cont ($bool-val (zero? (expval->num val)))))        
      (car-unop () (apply-cont cont (car (expval->list val))))
      (cdr-unop () (apply-cont cont ($list-val (cdr (expval->list val)))))
      (null?-unop () (apply-cont cont ($bool-val (null? (expval->list val)))))
      (print-unop () (begin
                       (eopl:printf "~a~%" (expval->num val))
                       (apply-cont cont ($stub 'PRINT_RET))))
      ))

  (define (run ticks src)
    (value-of-program ticks (scan&parse src)))

  ; ==========================================================================================================
  (define fresh-thread-id
    (let [(sn -1)]
      (lambda ()
        (set! sn (+ 1 sn))
        sn)))

  (define (remove-first pred q)
    (let loop ([lst '()]
               [q q])
      (if (null? q)
          #f
          (let ([head (car q)]
                [tail (cdr q)])
            (if (pred head)
                (append (reverse lst) tail)
                (loop (cons head lst) tail))))))
  ; --------------------------------
  
  (define (new-thread proc1)
    (let* [(thread-id (fresh-thread-id))
           (th ($a-thread thread-id (lambda ()
                                      (apply-procedure/k proc1 ($num-val thread-id) ($end-cont-subthread)))))]
      (set-list-of-threads! (cons th %list-of-threads))
      th))

  (define is-of-id
    (lambda (id)
      (lambda (th)
        (= (thread->id th) id))))

  (define (remove-thread id)
    (let ([result (remove-first (is-of-id id) %list-of-threads)])
      (if result
          (set-list-of-threads! result)
          (eopl:error 'remove-thread "failed to remove thread of id ~s" id))))
  ; ==========================================================================================================
  ; value-of-program : Int * Program -> ExpVal     
  (define (value-of-program timeslice pgm)
    (initialize-store!)    
    (cases program pgm
      (a-program (exp1)
                 (let [(th ($a-thread (fresh-thread-id) (lambda ()
                                                          (eval/k exp1 (init-env) ($end-cont-main-thread)))))]
                   (initialize-scheduler! timeslice th)
                   ((thread->proc th))))))
  
  ; value-of/k : Exp * Env * Cont -> FinalAnswer  
  (define (eval/k exp env cont)
    (when (@debug)
      (eopl:printf "           eval/k : ~s~n" (get-exp-variant-name exp)))
      
    (cases expression exp
      (const-exp (num)
                 (apply-cont cont ($num-val num)))
      (var-exp (var)
               (apply-cont cont (deref (apply-env env var))))
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env))))
      (let-exp (var exp1 body)
               (eval/k (call-exp (proc-exp var body) exp1) env cont))
      (letrec-exp (p-names b-vars p-bodies letrec-body)
                  (eval/k letrec-body ($extend-env-rec* p-names b-vars p-bodies env) cont))
      (const-list-exp (nums)
                      (apply-cont cont ($list-val (map $num-val nums))))
      (diff-exp (exp1 exp2)
                (eval/k exp1 env ($diff1-cont exp2 env cont)))
      (if-exp (exp1 exp2 exp3)
              (eval/k exp1 env ($if-test-cont exp2 exp3 env cont)))
      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont)))
      (begin-exp (exp exps)    
                 (if (null? exps)
                     (eval/k exp env cont)
                     (eval/k (call-exp (proc-exp (fresh-identifier 'dummy) (begin-exp (car exps) (cdr exps))) exp) env cont)))
      (set-exp (id exp)
               (eval/k exp env ($set-rhs-cont (apply-env env id) cont)))
      (unop-exp (unop1 exp)
                (eval/k exp env ($unop-arg-cont unop1 cont)))
      ; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
      (spawn-exp (exp)
                 (eval/k exp env ($spawn-cont cont)))
      ; ----------------
      (mutex-exp ()
                 (apply-cont cont ($mutex-val (new-mutex))))
      (wait-exp (exp)
                (eval/k exp env ($wait-cont cont)))
      (signal-exp (exp)
                  (eval/k exp env ($signal-cont cont)))
      ; ----------------
      (yield-exp ()
                 (place-on-ready-queue! ($a-thread %current-thread-id (lambda ()  ; ██
                                                                        (apply-cont cont ($stub 'YIELD-RET)))))
                 (run-next-thread))
      ))
  ; apply-cont : Cont * Exp -> FinalAnswer  
  (define (apply-cont k VAL)
    (when (@debug) (eopl:printf "           apply-cont : ~s~n" (get-cont-variant-name k)))
    
    (if (time-expired?)
        (begin ; time expired !
          (place-on-ready-queue! ($a-thread %current-thread-id (lambda () ; ██
                                                                 (apply-cont k VAL))))
          (run-next-thread))  
        (begin ; time remains ~
          (decrement-timer!)
          
          (cases Continuation k       
            ($spawn-cont (cont)
                         (let* [(f (expval->proc VAL))
                                (th (new-thread f))]
                           (place-on-ready-queue! th) ; ██                                                
                           (apply-cont cont ($num-val (thread->id th)))))
            ; ---------------------
            ($wait-cont (cont)
                        (let [(mut (expval->mutex VAL))]
                          (access-mutex mut (lambda () ; ░░
                                              (apply-cont cont ($stub 'WAIT-RET))))))

            ($signal-cont (cont)
                          (let [(mut (expval->mutex VAL))]
                            (leave-mutex mut (lambda () ; ░░
                                               (apply-cont cont ($stub 'SIGNAL-RET))))))
            ; ----------------------  
            ($end-cont-main-thread ()
                                   (set-final-answer! VAL)
                                   (remove-thread %current-thread-id)
                                   (run-next-thread))  
            ($end-cont-subthread ()
                                 (remove-thread %current-thread-id)
                                 (run-next-thread))

            ; ///////////////////////////////////////////////////////////////////////////////////////////////////
            ; unary-op
            ($unop-arg-cont (unop1 cont)
                            (apply-unop/k unop1 VAL cont))
            ; diff-exp
            ($diff1-cont (exp2 saved-env saved-cont)
                         (eval/k exp2 saved-env ($diff2-cont VAL saved-cont)))
            ($diff2-cont (val1 saved-cont)
                         (let ((n1 (expval->num val1))
                               (n2 (expval->num VAL)))
                           (apply-cont saved-cont ($num-val (- n1 n2)))))
            ; if
            ($if-test-cont (exp2 exp3 env cont)
                           (if (expval->bool VAL)
                               (eval/k exp2 env cont)
                               (eval/k exp3 env cont)))
            ; call-exp
            ($rator-cont (rand saved-env saved-cont)
                         (eval/k rand saved-env ($rand-cont VAL saved-cont)))
            ($rand-cont (val1 saved-cont)
                        (let ((f (expval->proc val1)))
                          (apply-procedure/k f VAL saved-cont)))
            ; set
            ($set-rhs-cont (loc cont)
                           (begin
                             (setref! loc VAL)
                             (apply-cont cont ($stub 'SET-RET))))
            ))))

  )

