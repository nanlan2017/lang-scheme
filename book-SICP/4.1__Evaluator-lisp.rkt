#lang racket

;; 一个exp都是在一个env中才有值
;; 定义变量/函数的过程，就是创建/修改 Env实体的过程
(define (eval exp env)
  (cond
    ;; 3  | "abc"
    [(self-evaluating? exp) exp]
    ;; x
    [(varible? exp) (lookup-varible-value exp env)]
    ;; '(1 x)
    [(quoted? exp) (text-of-quotation exp)]
    
    ;; set! = 
    [(assignment? exp) (eval-assignment exp env)]
    ;; define
    [(definition? exp) (eval-definition exp env)]
    ;; if test a b
    [(if? expr) (eval-if exp env)]
    ;; λ () ()
    [(lambda? exp)
     (make-procedure (lambda-parameters exp) (lambda-body exp) env)]

    ;; begin ...
    [(begin? exp) (eval-sequence (begin-actions exp) env)]
    ;; cond ...
    [(cond? exp) (eval (cond->if exp) env)]
    ;; (f args)
    [(application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env))]
    [else (error "Unknown Expression Type ---EVAL" exp)]))




(define (apply proc args)
  (cond
    [(primitive-procedure? proc) (apply-primitive-procedure proc args)]
    ;; 先扩展Env (加入 {形式参数变量<-> 实参值} 的绑定)
    ;; 再在此扩展后的环境中对 函数body 的Expr sequences 求值
    [(compound-procedure? proc)
     (eval-sequence (procedure-body proc)
                    (extend-env (procedure-parameters proc) args (procedure-environment proc)))]
    [else (error "Unknown Expression Type ---APPLY" proc)]))
    
;;=============================================================================
;; 获取实参列表
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


;; if 语句的求值
(define (eval-if exp env)
  (if (true? (eval (if-predict exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;=============================================================================
(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (eval (first-exp exps) env)]
    [else (eval (first-exp env)





















  
  