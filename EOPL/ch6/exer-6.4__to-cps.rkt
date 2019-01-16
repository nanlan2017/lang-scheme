#lang eopl

(define the-end-cont
  (lambda (val)
    (begin (eopl:printf "End of computation.~%")
           val)))
;===============================================================================
; origin
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))
; -------------------------------------------------------------------------------  CPS (ds-rep)
; (remove-first-ds 'a '(b c d e a g a h)) 
(define remove-first-ds
  (let ()
    (define-datatype Continuation Continuation?
      ($end-cont)       
      ($rf-cont
       (symbols symbol?)
       (cont Continuation?)
       ))

    (define (apply-cont k VAL)
      (cases Continuation k
        ($end-cont ()
                   (the-end-cont VAL))
        ($rf-cont (h cont)
                  (apply-cont cont (cons h VAL)))
        ))
    ; ```````````````````
    (define (rf/k s los cont)
      (if (null? los)
          (apply-cont cont '())
          (if (eqv? (car los) s)
              (apply-cont cont (cdr los))
              (rf/k s (cdr los) ($rf-cont (car los) cont)))))
    
    (define (remove-first s los)
      (rf/k s los ($end-cont)))
    
    remove-first))
; -------------------------------------------------------------------------------  CPS (proc-rep)
; (remove-first-proc 'a '(b c d e a g a h)) 
(define remove-first-proc
  (let ()
    (define ($end-cont)
      (lambda (VAL)
        (the-end-cont VAL)))
    (define ($rf-cont h cont)
      (lambda (VAL)
        (apply-cont cont (cons h VAL))))
    
    (define (apply-cont cont val)
      (cont val))
    ; ```````````````````
    (define (rf/k s los cont)
      (if (null? los)
          (apply-cont cont '())
          (if (eqv? (car los) s)
              (apply-cont cont (cdr los))
              (rf/k s (cdr los) ($rf-cont (car los) cont)))))
    
    (define (remove-first s los)
      (rf/k s los ($end-cont)))
    
    remove-first))
; -------------------------------------------------------------------------------  inlined

; =================================================================================