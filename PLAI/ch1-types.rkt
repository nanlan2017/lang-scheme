#lang plai-typed

(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

(define ma1 : MisspelledAnimal (caml 2))
(define ma2 : MisspelledAnimal (yacc 1.9))


(define (good? [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma
    [caml (humps) (>= humps 2)]    ;; 没必要同字段名（按位置匹配）
    [yacc (height) (> height 2.1)]))

;; 自动生成的类型判断 和 字段extractors
(caml? ma1)
(define v1 (caml-humps ma1))

;(good? 4)
;;-------------------------------------------test
(test (good? ma1)
      #t)

;;-------------------------------------------类型区分：s-expression | symbol/number/string | list
'+         ; symbol
(define l '(+ 1 2))   ; s-expression    ;; quote : 用于创建 s-exp类型 
;(first l)  ; l should be List
(first (s-exp->list l))                 ;;  list 已经被改写：只能接收同类型的operands  --> List<T>


;;     '(+  1 2)   这个literal 的类型可能是 s-exp  ， 也可能是 listof s-exp



