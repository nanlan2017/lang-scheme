#lang plai-typed

;; 算术运算的语法树节点类型（只支持 + , *)
(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])


;; 现在：输入不是String Text，而是就是 s-exp 类型。
;; 我们就可以通过类型解析，来构建AST
(define (parse [s : s-expression])
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))     ;; case (symbol/val)
         [(+)
          (plusC (parse (second sl)) (parse (third sl)))]
         [(*)
          (multC (parse (second sl)) (parse (third sl)))]
         [else
           (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

;; test
(parse '(+ (* 1 2) (+ 6 3)))
(plusC
 (multC (numC 1)
        (numC 2))
 (plusC (numC 6)
        (numC 3)))



(define (interp [s : s-expression]) : number
  (eval (parse s)))

(define (eval [exp : ArithC]) : number
  (type-case ArithC exp          ;; type-case 
    [numC (n)
          n]
    [plusC (l r)
           (+ (eval l) (eval r))]
    [multC (l r)
           (* (eval l) (eval r))]))
 
;; test
(test (interp '(+ (* 1 2) (+ 6 3))) 11)
;;==================================================================
;; core : ArithC
;; user-surface : ArithS
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)])

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    ;; x - y = x + (-1) * y
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]))





























