#lang racket
;;**************************************************  3.1 语法扩展 **********************************************
(define lazy
  (lambda (t)
    (let ([val #f] [flag #f])
      ;;  返回一个带闭包状态的无参函数对象。——第一次被调用时，才会求值
      (lambda ()
        ; 第一次被调用时，会被初次求值（ flag是标志是否已求值）
        ; 以后调用时，就直接打印已求值出的val了
        (if (not flag)
            (begin (set! val (t))
                   (set! flag #t))
            #'to-else)
        val))))

(define p
  (lazy (lambda ()
          (display "Ouch!")
          (newline)
          "got me")))

#|
> p
#<procedure:...p/TSPL/chap1.rkt:7:6>
> (p)    // 第一次调用
Ouch!
"got me"
> (p)    // 第二次调用（不打印ouch了）
"got me"
> 
|#


(define-syntax %let
  (syntax-rules ()
    ;; [ ] 表示这里可以 使用多种pattern来匹配 let后 接的东西
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

(%let ((a 1) (b 2)) (+ a b))

(define-syntax %and
  (syntax-rules ()
    [(_)
     #t]
    
    [(_ e)
     e]
    
    [(_ e1 e2 e3 ...)
     (if e1 (%and e2 e3 ...) #f)]))


(%and)
(%and 'haha)
(%and 'a 'b 'c)

(%let ([x (memv 'a '(a c))])
  (%and x (memv 'b x)))

(define-syntax %let*
  (syntax-rules ()
    [
     (_ ((x1 e1)) b1 ...)
     (%let ((x1 e1)) b1 ...)
    ]
    [    
     (_ ((x1 e1) (x2 e2) ...) b1 b2 ...)
     (%let ((x1 e1)) (%let* ((x2 e2) ...) b1 b2 ...))
    ]))

(%let* ((x1 1) (x2 (+ x1 1)) (x3 (+ x2 1))) (list x1 x2 x3))


(define-syntax %when
  (syntax-rules ()
    [
     (_ test e1 e2 ...)
     (if test (begin e1 e2 ...) 'unspecified)
    ]))

(define-syntax %unless
  (syntax-rules ()
    [
     (_ test e1 ...)
     (%when (not test) e1 ...)
    ]))

(let ([x 3])
  (%unless (= x 0) (set! x (+ x 1)))
  (%when (= x 4) (set! x (* x 2)))
  x)
;;**************************************************  3.2 more recursion **********************************************
;;  named let ,   letrec ,   尾递归,


;; 如何在局部（let绑定里）定义一个递归的函数
(let ([sum
       (λ (sum ls)
             (if (null? ls)
                 0
                 (+ (car ls) (sum sum (cdr ls)))))])
  (sum sum '(1 2 3 4 5))) ;;  显式的 把自身作为参数，从而可以完成 递归调用

;;  letrec
(letrec ([sum
          (λ (ls)
                (if (null? ls)
                    0
                    (+ (car ls) (sum (cdr ls)))))])
  (sum '(1 2 3 4 5)))

;;  even? 和 odd? 互相递归
(letrec ([even?
          (lambda (x)
            (or (= x 0)
                (odd? (- x 1))))]
         
         [odd?
          (lambda (x)
            (and (not (= x 0))
                 (even? (- x 1))))])
  (list (even? 20) (odd? 20)))
;; letrec 中， 【val expr】这一段首先要被eval， 如果绑定函数的话一般要用lambda形式，
(letrec ([f (lambda () (+ x 2))]  ;; 此时并不求值y, 知道在 body中求值 (f)时 才求值f
         [x 1])
  (f))


;(letrec ([y (+ x 2)]   ;; 此时需要求值y，可发现x还未定义
;         [x 1])
;  y)

;;------------- named let ----------------------------
(define list?
  (lambda (x)
    (letrec ([race
              (lambda (h t)
                ;; λ-body
                (if (pair? h)
                    (let ([h (cdr h)])
                      (if (pair? h)
                          (and (not (eq? h t))
                               (race (cdr h) (cdr t)))
                          (null? h)))                    
                    (null? h))
                )])
      (race x x)))) ;; 注意：let 绑定的 递归函数 只在Body中被使用了一次  When a recursive procedure is called in only one place outside the procedure

(define list?-2
  (lambda (x)
    (let race ([h x] [t x])
      ;; λ-body
      (if (pair? h)
          (let ([h (cdr h)])
            (if (pair? h)
                (and (not (eq? h t))
                     (race (cdr h) (cdr t)))
                (null? h)))
          (null? h))
      )))

#|
(let f-name ([v expr] ...)
  body)
-->
这是f-name是一个lambda，它的变量为v， 此时把 expr作为实参调用他 （注意 f-name必须是一个 递归函数）

(letrec ((f-name (λ (v) body)))
  (f-name expr))


((letrec ((name (lambda (var ...) body1 body2 ...)))
   name)
 expr ...)
|#
;;************************8
(define product
  (lambda (ls)
    
    (call/cc
      (lambda (break)
        
        (let f ([ls ls])
          (cond
            [(null? ls) 1]
            [(= (car ls) 0) (break 0)]
            [else (* (car ls) (f (cdr ls)))]))))))

(product '(1 2 3 4 5)) ; 120
(product '(7 3 8 0 1 9 5)) ; 0

(call/cc
  (lambda (k)
    (* 5 4)))

(define cc #f)

;; @@@@@@@@@@@@@    return 这个continuation，其实是  (call/cc ...) 它外面的东西：   (+  CALL/CC  (+ 1 7))

;;       continuation 对象：  封装的是一段计算（针对一个值）

;;  所有，要理解 continuation  这种类型的对象：  其实它就是一个 procedure ,且只接受一个参数，  （很像是一个闭包）
;;      1.  如何创建：
;;                  ① 通过   ... (call/cc (λ (cc) ###)) ...  可以获取到 该call/cc 外面的那个 continuation
;;      2.  ★ 一旦一个 continuation 被调用， 会立刻返回结果到 call/cc处  ===>  non-local exit  (Exit Continuation)
;;      3.  如何传递、使用该continuation，-----一旦创建得到一个continuation后， 就可以当做一个函数对象去传递 
(+ (call/cc (lambda (return)
                (set! cc return)
                (* 2 3)))
   (+ 1 7))

(+ (call/cc (lambda (return)
                (set! cc return)
                (cc 0)
                (* 2 3)))
   (+ 1 7))
;;**************************************************************************************************************************************
(define (hefty-computation do-other-stuff) 
    (let loop ((n 5)) 
      (display "Hefty computation: ") 
      (display n) 
      (newline) 
      (set! do-other-stuff (call/cc do-other-stuff)) ;; call/cc Procedure(1p) :  把控制权转移到该Procedure (有状态的计算）--- 并且把自身 作为 continuation传给后续计算
                                                     ;; ▇▇▇▇▇▇▇▇ call/cc + 单参函数/lambda --->  会自动捕捉(capture) call/cc外的 剩余计算 绑定到 一个实参 (类型为continuation)、对该单参函数就行调用
      (display "Hefty computation (b)")  
      (newline) 
      (set! do-other-stuff (call/cc do-other-stuff)) 
      (display "Hefty computation (c)") 
      (newline) 
      (set! do-other-stuff (call/cc do-other-stuff)) 
      (if (> n 0) 
          (loop (- n 1))
          'else-haha))) 

;; notionally displays a clock 
 (define (superfluous-computation do-other-stuff) 
    (let loop () 
      (for-each (lambda (graphic) 
                  (display graphic) 
                  (newline) 
                  (set! do-other-stuff (call/cc do-other-stuff))) 
                '("Straight up." "Quarter after." "Half past."  "Quarter til.")) 
      (loop)))


(define f
  (λ (cc)
    (display "f-part-1...")
    (set! cc (call/cc cc))
    (display "f-part-2...")))

(define g
  (λ (cc)
    (display "g-part-1...")
    (set! cc (call/cc cc))
    (display "g-part-2...")))
#|
> (f g)
f-part-1...g-part-1...f-part-2...
> (g f)
g-part-1...f-part-1...g-part-2...
> 
|#

(define capt-cc #f) 
  
(+ 100 (call/cc 
        (lambda (cont) 
          (set! capt-cc cont) 
          1))) 

;;------------------------------------------------
#|
又是一个非本地退出的例子，值得注意的是 control-state，这个函数第一次被调用的时候它还是个函数，

但是看注释“!!!”的这一行，经过第一次调用后， control-state 就把自己改成了一个 continuation，随后借助 generator 传进来的 return，完成一次非本地退出（▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇在 control-state 里退出 generator）。

第一次看这个可能有点晕，因为这里有两个 call/cc，而且还是嵌套的。
这两个 call/cc 各司其职，
    generator 里的 call/cc 是为了非本地退出，
    第二个 call/cc 则是为了记录遍历列表的状态——这样下一次调用 generator 的时候，通过 control-state（它是个 continuation），就能直接从上次修改 continuation 的地方继续运行，从而跳了回去，达到生成器的效果。

通过在 call/cc 内 apply continuation，我们可以在任意时刻从函数中跳出来；
通过修改中途获取的 continuation，我们还可以跳回去。
|#

(define (generate-one-element-at-a-time lst)
  ;; Hand the next item from a-list to "return" or an end-of-list marker
  (define (control-state return)
    (for-each
     (lambda (element)
               (set! return (call/cc
                              (lambda (resume-here)
                                ;; Grab the current continuation
                               (set! control-state resume-here)   ;;<-----------------
                               (return element)))))
     lst)
    (return 'you-fell-off-the-end))
  
  ;; This is the actual generator, producing one item from a-list at a time
  (define (generator)
    (call/cc control-state))
  
  ;; Return the generator
  generator)

(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))


;; -----------------------------------------
;; 用于获取到当前的continuation
(define (getcc)
  (call/cc (λ (c) c)))













































                      














