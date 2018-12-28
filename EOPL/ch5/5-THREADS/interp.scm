(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "lang.scm")
  (require "data-structures.scm")
  (require "store.scm")
  
  (require "scheduler.scm")
  (require "semaphores.scm")     
  
  (provide value-of-program
           trace-interp)
  ; ===============================================================  
  ; setting option
  (define trace-interp (make-parameter #f))
  
  ; =============================================================== the interpreter
  ;; value-of-program : Program * Int -> ExpVal
  ;; Page: 185    
  (define value-of-program
    (lambda (timeslice pgm)
      (initialize-store!)
      (initialize-scheduler! timeslice)
      (cases program pgm
        (a-program (exp1)
                   (☆value-of/k exp1 (init-env) (end-main-thread-cont))))))
   
  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page 182
  (define ☆value-of/k                    
    (lambda (exp env cont)
      (when (trace-interp) (eopl:printf "value-of/k: ~s~%" exp))
      (cases expression exp
        (const-exp (num)
                   (★apply-cont cont (num-val num)))

        (var-exp (var)
                 (★apply-cont cont (deref (apply-env env var))))
  
        (diff-exp (exp1 exp2)
                  (☆value-of/k exp1 env (diff1-cont exp2 env cont)))

        (if-exp (exp1 exp2 exp3)
                (☆value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))

        (proc-exp (var body)
                  (★apply-cont cont (proc-val (procedure var body env))))

        (call-exp (rator rand)
                  (☆value-of/k rator env (rator-cont rand env cont)))

        (let-exp (var exp1 body)          ; implemented like a macro!
                 (☆value-of/k (call-exp (proc-exp var body) exp1) env cont))
        
        (begin-exp (exp exps)           ; this one, too
                   (if (null? exps)
                       (☆value-of/k exp env cont)
                       (☆value-of/k
                        (call-exp
                         (proc-exp (fresh-identifier 'dummy) (begin-exp (car exps) (cdr exps)))
                         exp)
                        env cont)))
        
        (letrec-exp (p-names b-vars p-bodies letrec-body)
                    (☆value-of/k letrec-body (extend-env-rec* p-names b-vars p-bodies env) cont))

        (set-exp (id exp)
                 (☆value-of/k exp env
                              (set-rhs-cont (apply-env env id) cont)))
        
        (const-list-exp (nums)
                        (★apply-cont cont
                                     (list-val (map num-val nums))))
        
        (unop-exp (unop1 exp)
                  (☆value-of/k exp env
                               (unop-arg-cont unop1 cont)))
        ; -------------------------------------------------------
        (spawn-exp (exp)
                   (☆value-of/k exp env (spawn-cont cont)))

        (mutex-exp ()
                   (★apply-cont cont (mutex-val (new-mutex))))  

        (wait-exp (exp)
                  (☆value-of/k exp env (wait-cont cont)))

        (signal-exp (exp)
                    (☆value-of/k exp env (signal-cont cont)))

        (yield-exp ()
                   (place-on-ready-queue! (lambda () (★apply-cont cont (num-val 99))))
                   (run-next-thread))
        )))
  ; ===============================================================
  ;; apply-cont : Cont * Exp -> FinalAnswer
  ;; Page: 182 and 186
  (define ★apply-cont                    
    (lambda (%K %VALUE)
      (if (time-expired?)
          ; time expired !
          (begin
            (place-on-ready-queue! (lambda () (★apply-cont %K %VALUE)))  ;putting on the ready queue a thread that will try the apply-cont again
            (run-next-thread))
          ; time remains ~
          (begin
            (decrement-timer!)
            
            (cases continuation %K
              (end-main-thread-cont ()
                                    (set-final-answer! %VALUE)
                                    (run-next-thread))
  
              (end-subthread-cont ()
                                  (run-next-thread))
              ; ------------------------------------------------------------
              (spawn-cont (saved-cont)
                          (let ((proc1 (expval->proc %VALUE)))
                            (place-on-ready-queue! (lambda ()
                                                     (apply-procedure/k proc1 (num-val 28) (end-subthread-cont))))
                            (★apply-cont saved-cont (num-val 73))))

              (wait-cont (saved-cont)
                         (wait-for-mutex
                          (expval->mutex %VALUE)
                          (lambda () (★apply-cont saved-cont (num-val 52)))))

              (signal-cont (saved-cont)
                           (signal-mutex
                            (expval->mutex %VALUE)
                            (lambda () (★apply-cont saved-cont (num-val 53)))))

              ; ------------------------------------------------------------
              (unop-arg-cont (unop1 cont)
                             (apply-unop unop1 %VALUE cont))
              (diff1-cont (exp2 saved-env saved-cont)
                          (☆value-of/k exp2 saved-env (diff2-cont %VALUE saved-cont)))
              (diff2-cont (val1 saved-cont)
                          (let ((n1 (expval->num val1))
                                (n2 (expval->num %VALUE)))
                            (★apply-cont saved-cont
                                         (num-val (- n1 n2)))))
              (if-test-cont (exp2 exp3 env cont)
                            (if (expval->bool %VALUE)
                                (☆value-of/k exp2 env cont)
                                (☆value-of/k exp3 env cont)))
              (rator-cont (rand saved-env saved-cont)
                          (☆value-of/k rand saved-env
                                       (rand-cont %VALUE saved-cont)))
              (rand-cont (val1 saved-cont)
                         (let ((proc (expval->proc val1)))
                           (apply-procedure/k proc %VALUE saved-cont)))
              (set-rhs-cont (loc cont)
                            (begin
                              (setref! loc %VALUE)
                              (★apply-cont cont (num-val 26))))
              )))))
  ; ===============================================================
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
                   (☆value-of/k body (extend-env var (newref arg) saved-env) cont)))))
  ; ===============================================================
  (define apply-unop
    (lambda (unop1 arg cont)
      (cases unop unop1

        (zero?-unop ()
                    (★apply-cont cont
                                 (bool-val
                                  (zero? (expval->num arg)))))

        (car-unop ()
                  (let ((lst (expval->list arg)))
                    (★apply-cont cont (car lst))))
        (cdr-unop ()
                  (let ((lst (expval->list arg)))
                    (★apply-cont cont (list-val (cdr lst)))))

        (null?-unop ()
                    (★apply-cont cont 
                                 (bool-val (null? (expval->list arg)))))

        (print-unop ()
                    (begin
                      (eopl:printf "~a~%" (expval->num arg))
                      (★apply-cont cont (num-val 1))))

        )))

  )

