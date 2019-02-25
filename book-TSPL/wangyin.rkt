#lang racket

(define (treesum tree)
  (cond
    ((null? tree) 0)
    ((not (pair? (car tree))) (+ (car tree) (treesum (cdr tree))))
    (else (+ (treesum (car tree)) (treesum (cdr tree))))))

(treesum '((1 2) (3 4)))
 
(define tree-sum
  (lambda (exp)
    (match exp                         ; 对输入exp进行模式匹配
      [(? number? x) x]                ; exp是一个数x吗?如果是,那么返回这个数x
      [`(,e1 ,e2)                      ; exp是一个含有两棵子树的中间节点吗？
       (let ([v1 (tree-sum e1)]        ; 递归调用tree-sum自己，对左子树e1求值
             [v2 (tree-sum e2)])       ; 递归调用tree-sum自己，对右子树e2求值
         (+ v1 v2))])))                ; 返回左右子树结果v1和v2的和

 
(define calc
  (lambda (exp)
    (match exp                                ; 分支匹配:表达式的两种情况
      [(? number? x) x]                       ; 是数字，直接返回
      [`(,op ,e1 ,e2)                         ; 匹配提取操作符op和两个操作数e1,e2
       (let ([v1 (calc e1)]                   ; 递归调用 calc 自己，得到 e1 的值
             [v2 (calc e2)])                  ; 递归调用 calc 自己，得到 e2 的值
         (match op                            ; 分支匹配：操作符 op 的 4 种情况
           ['+ (+ v1 v2)]                     ; 如果是加号，输出结果为 (+ v1 v2)
           ['- (- v1 v2)]                     ; 如果是减号，乘号，除号，相似的处理
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))
#|
变量:x
函数：(lambda (x) e)
绑定：(let ([x e1]) e2)
调用：(e1 e2)
算术：(• e2 e2)
|#
 
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
(define interp
  (lambda (exp env)
    (match exp                                          ; 对exp进行模式匹配
      [(? symbol? x)                                    ; 变量
       (let ([v (lookup x env)])
         (cond
           [(not v) (error "undefined variable" x)]
           [else v]))]
      
      [(? number? x) x]                                 ; 数字
      
      [`(lambda (,x) ,e)                                ; 函数
       (Closure exp env)]
      
      [`(let ([,x ,e1]) ,e2)                            ; 绑定 -- 把绑定添加到env中
       (let ([v1 (interp e1 env)])
         (interp e2 (ext-env x v1 env)))]
      
      [`(,e1 ,e2)                                       ; 调用 -- (λ (x) exp) v0  相当于: 在env中添加绑定 x <-> v0 、再求值 exp
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (interp e (ext-env x v2 env-save))]))]
      
      [`(,op ,e1 ,e2)                                   ; 算术表达式
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))
 
;; 解释器的“用户界面”函数。它把 interp 包装起来，掩盖第二个参数，初始值为 env0
(define r2
  (lambda (exp)
    (interp exp env0)))

;; 测试
(r2 '(+ 1 2))
;; => 3
 
(r2 '(* 2 3))
;; => 6
 
(r2 '(* 2 (+ 3 4)))
;; => 14
 
(r2 '(* (+ 1 2) (+ 3 4)))
;; => 21
 
(r2 '((lambda (x) (* 2 x)) 3))
;; => 6
 
(r2
'(let ([x 2])
   (let ([f (lambda (y) (* x y))])
     (f 3))))
;; => 6
 
(r2
'(let ([x 2])
   (let ([f (lambda (y) (* x y))])
     (let ([x 4])
       (f 3)))))
;; => 6

