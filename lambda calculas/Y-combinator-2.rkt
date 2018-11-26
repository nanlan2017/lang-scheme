#lang racket


我现在在λ calculas里需要定义一个匿名的可调用的：(所有函数都没有名字，都要被其 λ 替代)
#square 4
(λ x . x * x) 4

#meta-power 5
(λ f x . $IF-ELSE (n==0) 1  x*(f (n-1)))
(λ f x . $IF-ELSE (n==0) 1  x*(f (n-1))) (λ f x . $IF-ELSE (n==0) 1  x*(f (n-1))) 5   #Error:  m-power m-power 5
#self-power 5
(λ f x . $IF-ELSE (n==0) 1  x*(f f (n-1)))
(λ f x . $IF-ELSE (n==0) 1  x*(f f (n-1))) (λ f x . $IF-ELSE (n==0) 1  x*(f f (n-1))) 5   

#Y
(λ y . (λ x . y (x x)) (λ x . y (x x)))

;;======================================================================

;; 不能这样递归定义
let %power = \n -> if-else n==0 1 n*power(n-1)

let meta-power = \self n -> if-else n==0 1 n*self(n-1)   ;; power自身作为自己的参数
# meta-power %power     5 ---> 120
# meta-power meta-power 5 ---> #Error

let self-power = \self n -> if-else n==0 1 n*(self self (n-1))
# self-power self-power 5 ---> 120