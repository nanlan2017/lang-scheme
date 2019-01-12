(module data-structures-continuation (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  ;;============================================================ Continuation
  ; Continuation就是数据结构化的 CPS 中的后续函数！
  
  ;      (if e1 then e2 else e3) #cont
  ;           eval e1 env \v1->
  ;               if v1 then eval e1 env #cont
  ;                     else eval e2 env #cont
  
  ; 但是采用了 procedural-rep (under  apply-cont)
  (define-datatype Continuation Continuation?
    ($end-cont)

    ; Exception
    ($try-cont
     (cvar identifier?)
     (handler-exp expression?)
     (env Env?)
     (cont Continuation?))
    ($raise1-cont
     (cont Continuation?))
    ;````````````````````````````````
    ; unary : zero? | null? car cdr
    ($unary-arg-cont
     (op unary-op?)
     (cont Continuation?))
    
    ; if-exp
    ($if-test-cont
     (then-exp expression?)
     (else-exp expression?)
     (env Env?)
     (cont Continuation?))
    
    ; diff-exp
    ($diff1-cont
     (e2 expression?)
     (env Env?)
     (cont Continuation?))
    ($diff2-cont
     (v1 ExpVal?)
     (cont Continuation?))
    
    ; call-exp
    ($rator-cont
     (rand-exp expression?)
     (env Env?)
     (cont Continuation?))
    ($rand-cont
     (f-expval ExpVal?)
     (cont Continuation?))
    )
  )
