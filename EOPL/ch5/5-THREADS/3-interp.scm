(module interp (lib "eopl.ss" "eopl")
  
  (require "0-lang.scm")
  (require "0-store.scm")
  (require "1-data-structures.scm")
  (require "1-data-structures-continuation.scm")    
  (require "2-scheduler.scm")
  (require "2-semaphores.scm")     
  
  (provide run
           value-of-program
           Option@-trace-interp

           ; imported
           get-store-as-list
           
           %ready-queue
           %final-answer
           %max-time-slice
           %time-remaining
           )
  ; ===============================================================  
  ; setting option
  (define Option@-trace-interp (make-parameter #f))

  ; ===============================================================
  (define (apply-procedure/k f argv cont)
    (cases Proc f
      ($procedure (var body saved-env)
                  (eval/k body ($extend-env var (newref argv) saved-env) cont))))

  (define (apply-unop unop1 val cont)
    (cases unop unop1
      (zero?-unop () (apply-cont cont ($bool-val (zero? (expval->num val)))))        
      (car-unop () (apply-cont cont (car (expval->list val))))
      (cdr-unop () (apply-cont cont ($list-val (cdr (expval->list val)))))
      (null?-unop () (apply-cont cont ($bool-val (null? (expval->list val)))))
      (print-unop () (begin
                       (eopl:printf "~a~%" (expval->num val))
                       (apply-cont cont ($num-val 1))))
      ))

  (define (run src ticks)
    (value-of-program (scan&parse src) ticks))

  ; value-of-program : Program * Int -> ExpVal     
  (define (value-of-program pgm timeslice)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp1)
                 (eval/k exp1 (init-env) ($end-cont-main-thread)))))
  ; ==========================================================================================================
  ; Page 182
  ; value-of/k : Exp * Env * Cont -> FinalAnswer  
  (define (eval/k exp env cont)
    (when (Option@-trace-interp)
      (eopl:printf "value-of/k: ~s~%" exp))
      
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
      ; `````````````````````````````````````````
      (diff-exp (exp1 exp2)
                (eval/k exp1 env ($diff1-cont exp2 env cont)))

      (if-exp (exp1 exp2 exp3)
              (eval/k exp1 env ($if-test-cont exp2 exp3 env cont)))

      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont)))

      (begin-exp (exp exps)    
                 (if (null? exps)
                     (eval/k exp env cont)
                     (eval/k (call-exp
                              (proc-exp (fresh-identifier 'dummy) (begin-exp (car exps) (cdr exps)))
                              exp)
                             env cont)))

      (set-exp (id exp)
               (eval/k exp env ($set-rhs-cont (apply-env env id) cont)))

      (unop-exp (unop1 exp)
                (eval/k exp env ($unop-arg-cont unop1 cont)))
      ; -------------------------------------------------------
      (spawn-exp (exp)
                 (eval/k exp env ($spawn-cont cont)))

      (wait-exp (exp)
                (eval/k exp env ($wait-cont cont)))

      (signal-exp (exp)
                  (eval/k exp env ($signal-cont cont)))

      (yield-exp ()
                 (place-on-ready-queue! (lambda ()
                                          (apply-cont cont ($num-val 99))))
                 (run-next-thread))

      (mutex-exp ()
                 (apply-cont cont ($mutex-val (new-mutex))))  
      ))
  ; ===============================================================
  ; Page: 182 and 186
  ; apply-cont : Cont * Exp -> FinalAnswer  
  (define (apply-cont k VAL)
    (if (time-expired?)
        ; time expired !
        (begin
          (place-on-ready-queue! (lambda ()
                                   (apply-cont k VAL)))  ;putting on the ready queue a thread that will try the apply-cont again
          (run-next-thread))
        ; time remains ~
        (begin
          (decrement-timer!)
            
          (cases Continuation k
            ($end-cont-main-thread ()
                                   (set-final-answer! VAL)
                                   (run-next-thread))
  
            ($end-cont-subthread ()
                                 (run-next-thread))
            ; ------------------------------------------------------------
            ($spawn-cont (cont)
                         (let ((f (expval->proc VAL)))
                           (place-on-ready-queue! (lambda ()
                                                    (apply-procedure/k f ($num-val 28) ($end-cont-subthread))))
                           (apply-cont cont ($num-val 73))))

            ($wait-cont (cont)
                        (wait-for-mutex (expval->mutex VAL) (lambda ()
                                                              (apply-cont cont ($num-val 52)))))

            ($signal-cont (cont)
                          (signal-mutex (expval->mutex VAL) (lambda ()
                                                              (apply-cont cont ($num-val 53)))))

            ; ------------------------------------------------------------
            ; unary-op
            ($unop-arg-cont (unop1 cont)
                            (apply-unop unop1 VAL cont))
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
                             (apply-cont cont ($num-val 26))))
            ))))

  )

