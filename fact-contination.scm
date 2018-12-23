(module fact-contination (lib "eopl.ss" "eopl")

  (define-datatype continuation continuation?
    (end-cont)  
    (fact1-cont
     (n integer?)
     (saved-cont continuation?)))

;  (define (apply cont val)
;    (cases continuation cont
;      (end-cont ()
;         val)
;      (fact1-cont (n saved-cont)
;         (apply saved-cont (* n val)))))


;  (define (fact n)
;    (fact/k n (end-cont)))
;
;  (define (fact/k n cont)
;    (if (zero? n)
;        (apply cont 1)
;        (fact/k (- n 1) (fact1-cont n cont))))

  ;;**********************  寄存器化 registerize ************************************

  
  ;  f {  g(n)  }   ====>  define global-n ,,  f { set! global-n n,, g() }   ,,  g { use global-n..}
  ;; 反正函数是一个接一个执行的，所以上一个函数——> 下一个函数所传递的参数 都可以约定好的global varible 来传递。

  
  (define $n 'uninitialized)
  (define $cont 'uninitialized)
  (define $val 'uninitialized)


  (define (apply)  ;; $cont, $val
    (cases continuation $cont
      (end-cont ()
         (eopl:printf "----- end of computation...--------~%")       
         $val)
      (fact1-cont (saved-n saved-cont)
         (set! $cont saved-cont)
         (set! $val (* saved-n $val))  ; <------ 
         (apply))))
           

  (define (fact n)
    (set! $cont (end-cont))
    (set! $n n)
    (fact/k))    ;; 准备调用fact/k ： 隐藏的参数是 $cont, $n

  (define (fact/k) ;; $n, $cont
    (if (zero? $n)
        (begin (set! $val 1)
               (apply))
        (begin (set! $cont (fact1-cont $n $cont))
               (set! $n (- $n 1))
               (fact/k))))
               
               
  )
