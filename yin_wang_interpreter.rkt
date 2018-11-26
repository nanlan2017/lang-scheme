#lang racket

;;;-----------------------------------------------------------
;;  变量: x
;;  函数：(lambda (x) e)
;;  绑定：(let ([x e1]) e2)
;;  调用：(e1 e2)
;;  算术：(• e2 e2)

 
;;; 以下三个定义 env0, ext-env, lookup 是对环境(environment)的基本操作:
 
;; 空环境
(define env0 '())
 
;; 扩展。对环境 env 进行扩展，把 x 映射到 v，得到一个新的环境
(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))
 
;; 查找。在环境中 env 中查找 x 的值。如果没找到就返回 #f
(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
       [(not p) #f]
       [else (cdr p)]))))
       
;; 闭包的数据结构定义，包含一个函数定义 f 和它定义时所在的环境
(struct Closure (f env))
 
;; 解释器的递归定义（接受两个参数，表达式 exp 和环境 env）
;; 共 5 种情况（变量，函数，绑定，调用，数字，算术表达式）
;; ▇▇▇▇ 这个eval只能求值“单个表达式”
(define eval
  (lambda (exp env)                                     ; 在env中求值expression
    (match exp                                          ; 对exp进行模式匹配
      
      [(? symbol? x)                                    ; 1.变量
       (let ([v (lookup x env)])
         (cond
          [(not v) (error "undefined variable" x)]
          [else v]))]
      
      [(? number? x) x]                                 ; 数字
      
      [`(lambda (,x) ,e)                                ; 函数  (λ (x) e)  ---> 创建一个闭包对象（封装Expr和Env本身）
       (Closure exp env)
       ]
      
      [`(let ([,x ,e1]) ,e2)                            ; 绑定  //# let会extend env -> eval in new Env
       (let ([v1 (eval e1 env)])
         (eval e2 (ext-env x v1 env)))]

      [`(letrec ([,x ,e1] [,y ,ex]) ,body)
       (let ([v1 (eval e1 env)])
         (let ([env1 (ext-env x v1 env)])
          (let ([v2 (eval ex env1)])
           (eval body (ext-env y v2 (ext-env y v2 env1))))))]
      
      [`(,e1 ,e2)                                       ; 调用  //((λ (x) (expr)) y)  // 就是 {x,y} 扩展到env, 然后eval expr in new Env
       (let ([v1 (eval e1 env)]
             [v2 (eval e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            ;; 词法（静态）作用域
;           (eval e (ext-env x v2 env-save))
            ;; 动态作用域
            (eval e (ext-env x v2 env))
            ]))]
      
      [`(,op ,e1 ,e2)                                   ; 算术表达式
       (let ([v1 (eval e1 env)]
             [v2 (eval e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))
 
;; 解释器的“用户界面”函数。它把 interp 包装起来，掩盖第二个参数，初始值为 env0
(define r2
  (lambda (exp)
    (eval exp env0)))

;;====================================================================================▇▇▇▇▇▇▇▇▇▇▇▇
;不要“害怕”函数 ：
;      函数不过是  (let ([args  params]...)
;                      expr1  expr2  expr3
;
;【函数定义】
;   （用函数的表达式本身来表示它的值)
; ———— 匿名的：则无法添加到Env中（无标识嘛）。 所以创建一个闭包对象即可。（要能封装 args + body-exprs || Env ）
; ———— 具名的：同上，不过是添加在Env中、需要时取出来               



;【函数应用 f 1 2】
;   (其实就是对f的body-exprs求值）
; ———— （0）获取f定义： 匿名的话就是闭包对象，否则就去Env查询               
; ———— （1）扩展环境 : args <--> params
; ————  (2) 求值Body : eval expr1; eval expr2;                

;;=====================================================================================
(r2 '((lambda (x) (+ 2 x)) 10))

(r2 '(let ([a 11])
       ((lambda (x) (+ x a)) 10)))

(r2 '(let ([x 2])
       (let ([f (lambda (y) (* x y))])   ;; ▇▇▇▇ 词法作用域（静态）： 说明该闭包创建时绑定的 Env是当前的Env , 而不是不绑定Env、在被调用处的Env求值
         (let ([x 4])                    ;; 为了实现 lexical scoping,我们必须把函数做成“闭包”(closure) —————即函数body-exprs 的自由变量值需基于当时的Env
           (f 3)))))                     ;; ——————>  那么C++啥的都依赖全部变量（自由变量），都属于 动态作用域！

(r2 '(letrec ([x 4] [y (+ x 2)])
       (+ x y)))
       










