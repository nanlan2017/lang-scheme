#lang racket


;;  church数 (f,x)  并不是“自然数”！
;; ▇▇▇▇ church数 只在 zero 为 自然数0、succ为自然数的 +1 时，才对应自然数！

;;  自然数  0     1   2    3       ( inc)
;;  丘奇数  zero  one two  three   (f  x )
(define zero
  (lambda (f) (lambda (x) x)))

(define one           ;;   one f x ---> f x
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define three
  (λ (f) (λ (x) (f (two f x)))))   ;; ???

;; 自然数2 就是 two inc 0;
;; -------> 传入inc和自然数0之后就能将丘奇数“翻译”成自然数


; 如何把 m 转为 church数（z,s)
(define m
  (lambda (f) (lambda (x)

                
;;; ▇▇▇▇ church数 的加法运算  === 显然也是要基于church数本就依赖的 f x 两个参数。
;  相当于用 0, succ 去定义自然数上的 +
                ;  (+ x y) =  x succ (y succ 0)
(define (ch-add m n)   ;;  m, n 都是 f x 的二元函数
  (λ (f) (λ (x)  