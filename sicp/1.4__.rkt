#lang racket

;; 练习1.5
;;  用一段程序，测试解释器使用 正则序/应用序

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;(test 0 (p))

;; 正则序: 先代入，再算
;(if (= 0 0) 0 (p)) --> 0  // (p)不会被求值
;; 应用序
;(p) 求值不出来，死循环？(而且是尾递归？)


;;=============================================================================================== 牛顿法求一个数的平方根
(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.01))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define ($sqrt x)
  (sqrt-iter 1.0 x))
;;===============================================================================================

;;  可变参数
;;(define (same-parity hd . nums)
;;  (define (same-eo x y)
;;    (= (% (- x y) 2) 0))
;;  (let ([tal (cond
;;               [(null? nums) '()]
;;               [(same-eo hd (car nums)) (cons (car nums) (same-parity hd (
        
;;  (cond
;;    [(null? nums) '(hd)]
;;    [else
(map (curry + 2) '(4 5))























     