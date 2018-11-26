#lang racket


(define zero
  (λ (s z) z))

(define one
  (λ (s z) (s z)))

(define two
  (λ (s z) (s (s z))))

(define three
  (λ (s z) (s (two s z))))    ;; 

;;------------------------------------------------------------  自然数
(define (inc x)
  (+ x 1))

(two inc 3)  ;; 5 ?
(three inc 3) 
;;------------------------------------------------------------

(define $0th
  (λ (s z) z))

(define $1th
  (λ (s z) (s z)))

(define $2th
  (λ (s z) (s (s z))))

(define $3th
  (λ (s z) (s ($2th s z))))

(define $4th
  (λ (s z) (s ($3th s z))))


($3th inc 3)   ;;  以自然数3 作为本church数列的 zero元，以inc作为 f , 可获取本 church数列的 第3个 c-num 为 6

;;------------------------------------------------------------

;;  定义 church 数的 加法
(define (@add mth nth)
  (λ (s z) (mth s (nth s z))))

(define $5th (@add $2th $3th))
($5th inc 10)

($4th ($2th inc 0) 6)  ;;  6+ 2*4 = 14

(define (@mult mth nth)
  (λ (s z) (nth (mth s z) z)))

(define $20th (@mult $4th $5th))
($20th inc 90)

;add 1th 2th
;(λ (s z) (1th s (2th s z)))
;(λ (s z)
;  ((λ (s z) (s (s z)))))

;;------------------------------------------------------------;;------------------------------------------------------------
(define $T
  (λ (x y) x))

(define $F
  (λ (x y) y))

(define $if-then-else
  (λ (condition t-exp f-exp)
    (condition t-exp f-exp)))  ;; condition 本就是二元函数

;; 只有b1,b2都为 $TRUE 时, 才是$TRUE (有一个FALSE,就为false)
(define ($and b1 b2) (b1 b2 $F)) 

($and $T $F)
($if-then-else ($and $T $F) 1 2)

;; 只有两个都FALSE时,才FALSE
(define ($or b1 b2) (b1 $T b2))   
($or $T $F)

(define ($not bo) (bo $F $T))

($not $T)
;;------------------------------------------------------------;;------------------------------------------------------------
;;lambda演算使用递归实现循环
;; 但是,由于在lambda演算里函数没有名字,我们得采取一些非常手段。这就是所谓的Y-combinator,又名lambda不动点运算符。

(define factorial
  (λ (n) (cond
           [(= n 0) 1]
           [#t (* n (factorial (- n 1)))])))  ;; 但这里的factorial不能出现

(factorial 5)

(define Y
  (λ (y)
    (λ (x) (y (x x)) (λ (x) (y (x x))))))








  














