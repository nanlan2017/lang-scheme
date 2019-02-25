#lang racket

;10条戒律
;
;5条Laws
;1.  eq?  只适用于 非number的 atom
;2.  null? = list-is-empty?  只用于判断list是否为'()
;
;equal?

;;****************************************************** chapter-1 *******************************************************************
;; 使得 空list 不属于 atom
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))
;;****************************************************** chapter-2 *******************************************************************

;; list-of-atoms?
(define (lat? exps)
  (cond
    ((null? exps) #t) ; 空列表 属于 list-of-atoms
    ((atom? (car exps)) (lat? (cdr exps)))
    (else #f)))

;; member-of-list?
(define member?
  (λ (x lst)
    (cond
      ((null? lst) #f)
      (else (or (eq? x (car lst))
                (member? x (cdr lst)))))))
;;****************************************************** chapter-3 *******************************************************************
;; remove the first occurence
(define rem-ber
  (λ (x lst)
    (cond
      ((null? lst) '())  ; 通过cdr递归到列表的末尾时，必然要遇到 '()
      ((eq? x (car lst)) (cdr lst))
      (else (cons (car lst) (rem-ber x (cdr lst)))))))

;; test````
(rem-ber 'mint '(lamb chops and mint flavored mint jelly))

;; collect the first element of s-exps in lists
(define firsts
  (λ (lst)
    (cond
      ; stop call next self when arrive at end
      ((null? lst) '())
      ; if the s-exp is atom
      ((atom? (car lst)) (cons (car lst) (firsts (cdr lst))))
      ; the s-exp is list
      (else (cons (caar lst) (firsts (cdr lst)))))))

;; test```
(firsts '((five plums) (four) (eleven green cats)))

;; insert one item after the first occurence of x
(define insertR
  (λ (newx oldx lst)
    (cond
      ((null? lst) '())
      ((eq? oldx (car lst)) (cons oldx (cons newx (cdr lst))))
      (else (cons (car lst) (insertR newx oldx (cdr lst)))))))

(insertR '-ang 'w '(h o w x j h))

(define insertL
  (λ (newx oldx lst)
    (cond
      ((null? lst) '())
      ((eq? oldx (car lst)) (cons newx lst))
      (else (cons (car lst) (insertL newx oldx (cdr lst)))))))

(insertL '-ang 'w '(h o w x j h))

(define subst
  (λ (newx oldx lst)
    (cond
      ((null? lst) '())
      ((eq? oldx (car lst)) (cons newx (cdr lst)))
      (else (cons (car lst) (subst newx oldx (cdr lst)))))))

(subst 'www 'w '(j h w i l you))

(define subst2
  (λ (newx oldx1 oldx2 lst)
    (cond
      ((null? lst) '())
      ((or (eq? oldx1 (car lst)) (eq? oldx2 (car lst))) (cons newx (cdr lst)))
      (else (cons (car lst) (subst2 newx oldx1 oldx2 (cdr lst)))))))

(subst2 'www 'w 'h '(j h w i l you))

;; 其实一直都是通过递归的方式 来迭代遍历 list的每个元素而已
(define multi-rember
  (λ (x lst)
    (cond
      ((null? lst) '())
      (else
        (cond
          ((eq? (car lst) x) (multi-rember x (cdr lst)))
          (else (cons (car lst) (multi-rember x (cdr lst)))))))))

(multi-rember 'w '(j h w i w you w ha))


(define multi-insertL
  (λ (newx oldx lst)
    (cond
      ((null? lst) '())
      (else
        (cond
          ((eq? oldx (car lst)) (cons newx (cons oldx (multi-insertL newx oldx (cdr lst)))))
          (else (cons (car lst) (multi-insertL newx oldx (cdr lst)))))))))

(multi-insertL '-ang 'w '(h o w x w j h w))

;;****************************************************** chapter-4 *******************************************************************
(define add1 (λ (x) (+ 1 x)))
(define sub1 (λ (x) (- x 1)))
(define zero? (λ (x) (eq? x 0)))

(define @+
  (λ (x y)
    (cond
      ((zero? y) x)
      (else (@+ (add1 x) (sub1 y)))))) ;; <--这是尾递归版本的   add1 (@+ x (sub1 y))

(define @-
  (λ (x y)
    (cond
      ((zero? y) x)
      (else (@- (sub1 x) (sub1 y)))))) 

; tup+  : 相加两个numbers-list
(define tup+
  (λ (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (@+ (car tup1 tup2) (tup+ (cdr tup1 tup2))))))))

; x : 相乘
(define @*
  (λ (x y)
    (cond
      ((zero? y) 0)
      (else (@+ x (@* x (sub1 y)))))))

;;  >  ,  <  ,  mod
; exp
(define @^
  (λ (x y)
    (cond
      ((zero? y) 1)
      (else (@* x (@^ x (sub1 y)))))))

(define @>
  (λ (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (@> (sub1 x) (sub1 y))))))

(define @<
  (λ (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) #t)
      (else (@< (sub1 x) (sub1 y))))))

(define @=
  (λ (x y)
    (not (or (@> x y) (@< x y)))))

; number?  x
;; eqan?  use eq? for non-numeric atom  ,  use @= for numeric atom
(define eqan?
  (λ (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (@= a1 a2))
      ((or  (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

;; occur 是一个典型的 可以推导出 尾递归的 题目
;;      ——把已有的 occur次数当做 occur递归每次携带进入下一栈的状态、而不是保存在每层的栈中！
(define occur
  (λ (x lst)
    (cond
      ((null? lst) 0)
      ((eq? x (car lst)) (add1 (occur x (cdr lst)))) ;; 这是非尾递归的 、内存消耗大
      (else (occur x (cdr lst))))))

(define occur!
  (λ (x lst n)
    (cond
      ((null? lst) n)
      ((eq? x (car lst)) (occur! x (cdr lst) (+ 1 n)))
      (else (occur! x (cdr lst) n)))))

(occur 'w '(1 3 5 w ang j k w love say w h))
(occur! 'w '(1 3 5 w ang j k w love say w h) 0)


;; remove-pick  : remove the element at
;;******************************************  chapter-5 ***************************************************************************************************
(define rember*
  (λ (x lst)
    (cond
      ((null? lst) '())
      ;; atom , =
      ((eq? x (car lst)) (rember* x (cdr lst)))
      ;; atom , !=
      ((atom? (car lst)) (cons (car lst) (rember* x (cdr lst))))
      ;; s-exp
      (else (cons (rember* x (car lst)) (rember* x (cdr lst)))))))

(rember* 'c '(c
              (b c)
              (d e (c))
              c
              (h i (j (k (c e))))))

(define subst*
  (λ (newx oldx lst)
    (cond
      ((null? lst) '())
      ((atom? (car lst)) (cond
                           ((eq? (car lst) oldx) (cons newx (subst* newx oldx (cdr lst))))
                           (else (cons (car lst) (subst* newx oldx (cdr lst))))))
      (else (cons (subst* newx oldx (car lst)) (subst* newx oldx (cdr lst)))))))

(subst* '--- 'MMM '((MMM)
                    (A (C (MMM B)))
                    MMM
                    (F G MMM (K MMM))))

(define member*
  (λ (x lst)
    (cond
      ((null? lst) #f)
      ((atom? (car lst)) (cond
                           ((eq? x (car lst)) #t)
                           (else (member* x (cdr lst)))))
      (else (or (member* x (car lst)) (member* x (cdr lst)))))))

(member* 'www '((a) b (c (d (www e))) g h))
          

(define left-most
  (λ (lst)
    (cond
      ((null? lst) '())
      ((atom? (car lst)) (car lst))
      (else (left-most (car lst))))))

(left-most '( ((a b) c)
              ((()))
              7
              (a (b c))))

;(and a b) == (cond
;               ((not a) #f)
;               (else b))
;
;(or a b) == (cond
;              (a a)
;              (else b))
                               
(define eqlist?
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2) #t))
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))  (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(a (() b (c (d))) (f g h))
         '(a (() b (c (d))) (f g h)))


;;  正如 eqan? 是  @= 和 eq? 的合体
;;      equal? 是 eqan? 和 eqlist? 的合体
(define equal?
  (λ (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))
;;******************************************  chapter-6 ***************************************************************************************************
;;  写一个可以 计算 中缀表达式的
(define numbered?
  (λ (aexp)
    (cond
      ;; '@
      ((atom? aexp) (number? aexp))
      ;; '(@)  : 用tail递归时必然会遇到 (@) -> ()
      ((null? (cdr aexp)) (number? (car aexp)))
      ;;  + x ^    --->  or a (or b c)
      ;;  M # N  :  m,n must be number
      ;;  car  cadr  cddr
      ((or (eq? (cadr aexp) '+) (or (eq? (cadr aexp) 'x) (eq? (cadr aexp) '^)))
       (and (numbered? (car aexp)) (numbered? (cddr aexp))))
      (else #f))))

(define numbered?!
  (λ (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and (numbered?! (car aexp))
             (numbered?! (caddr aexp)))))))
      
(numbered? '4)
(numbered? '(4 + 5 x a))
(numbered? '(12 ^ 3 + 2 x 4))

(numbered?! '4)
(numbered?! '(4 + 5 x a))  ;; 压根只看了 4 ，5 嘛！ 
(numbered?! '(12 ^ 3 + 2 x 4))


(define value
  (λ (exp)
    (cond
      ((not (numbered? exp)) (error '(this is not arthmetic expression)))
      ; '@
      ((atom? exp) exp)
      ; '(@)
      ((null? (cdr exp)) (value (car exp)))
      (else (cond
              ;;  ((2 x 3) + ...)  此时car 并不直接是个数字，需要value()
              ;; 错了，要考虑运算符优先级的！
              ((eq? (cadr exp) '+) (@+ (value (car exp)) (value (cddr exp))))
              ((eq? (cadr exp) 'x) (@* (value (car exp)) (value (cddr exp))))
              (else (@^ (value (car exp)) (value (cddr exp)))))))))

;(value '3)
;(value '(4))
;(value '(3 + 4))
;(value '(3 + 4 ^ 2 + 1 x 10))
;(value '(3 + 4 ^ 2 + 1 x 10a))

#|
> (value '(3 + 4 ^ 2 + 1 x 3))
1027
> (value '(3 + 4 ^ 2 + 1 x 10))
 xx  user break
> (value '(3 + 4 ^ 2 + 1 x 3))
1027
> (value '(3 + 4 ^ 2 + 1 x 4))
4099
> (value '(3 + 4 ^ 2 + 1 x 5))
16387
> (value '(3 + 4 ^ 2 + 1 x 6))      用了15秒才算出来，应该是递归栈过多！！
65539
|#
;;******************************************  chapter-7 ***************************************************************************************************
(define set?
  (λ (s)
    (cond
      ((null? s) #t)
      ((member? (car s) (cdr s)) #f)
      (else (set? (cdr s))))))

(define makeset
  (λ (lst)
    (cond
      ((null? lst) '())
      ((member? (car lst) (cdr lst)) (makeset (cdr lst)))
      (else (cons (car lst) (makeset (cdr lst)))))))

;(define makeset*
;  (λ (lst)
;    (cond
;      ((null? lst) '())
;      ((atom? (car lst)) (cons (car lst) (rember* (car lst) (cdr lst))))
;      (
;;******************************************  chapter-8 ***************************************************************************************************
;;  函数作为参数，   lambda ,  predict谓词，  continuation

;;  cont 是 multi-rember的 continuation
(define multi-rember-cont
  ;;  其延续 是一个接受 两个List作为 参数的函数
  (λ (a lat cont)
    (cond
      ((null? lat)       (cont '() '()))
      ((eq? a (car lat)) (multi-rember-cont a
                                            (cdr lat)
                                            (λ (newlat seen)
                                              (cont newlat
                                                     (cons (car lat) seen)))))  ; 这个 lat 是闭包变量 
      (else (multi-rember-cont a
                               (cdr lat)
                               (λ (newlat seen)
                                 (cont (cons (car lat) newlat)
                                       seen)))))))
;; ▇▇▇▇▇▇▇▇[在递归中传递闭包]▇▇▇▇▇▇▇▇▇▇ 这个例子很有内涵啊！   首先：这是尾递归 ，  其次， 它在递归中传递、更新着一个闭包  <----- 递归过程是吧 (a, lat) 这个栈中的 元素 往 闭包中移动
;;  1.  首先是 通过把 递归数据全部 传递， 变成尾递归  （closure用来记录状态）
;;  2.  
(define a-friend
  (λ (x y) (null? y)))  ;;  ▇▇▇▇ 这个函数对象 也在递归中被 更改！
;;  只要lat不是空列表（没遍历到末尾），就会进行一次递归。

;;  λ (l1 l2) (null? l2)
;;  eq:      λ (l1 l2) (f l1    ~l2~)   ----- 这个更新后的function成了闭包，它将(car lat) 携带，  append 一个元素后 才去调用 最初的 init-f 
;;  !eq:     λ (l1 l2) (f ~l1~   l2)     ----------▇▇▇ 每迭代过一个元素，就将这个元素 添加到 cont的 clousure上（即参数 l1 l2) 上 --------------- 最后直接调用init-f  作用于此 过滤形成的两个列表   (eq a x)

;;  当初始函数是：  null? l2
;;   一次A递归：   必然变#f
;;   一次B递归：   不影响
(define bfr
  (λ (l1 l2) (append l1 (cons '~~~~~ l2))))

(multi-rember-cont 'a '(a b c d a e g h a) a-friend)
(multi-rember-cont 'a '(b c d e g h) a-friend)
(multi-rember-cont 'a '(a b c d a e g h a) bfr)


(define multi-insert-LR
  (λ (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)    (cons new (cons oldL (multi-insert-LR new oldL oldR (cdr lat)))))   ;;  注意，这个不是尾递归
      ((eq? (car lat) oldR)    (cons oldR (cons new (multi-insert-LR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multi-insert-LR new oldL oldR (cdr lat)))))))

;; 将 '* 插入到 '- 的左边， 以及 '+的右边
(multi-insert-LR '* '- '+ '(3 4 - a b + k - + e))
                      ;;  '(3 4 * - a b + * k * - + * e)

;; closure 版：   闭包中的数据为 元素列表， 插入次数（左、右）
(define multi-insert-c
  (λ (new oldL oldR lat col)
    (cond
      ;;  ▇▇▇▇▇▇▇▇▇▇ 尾递归 ： 把一次递归中的处理， 完全 体现在 递归传递数据 的变化 上 。  
      ((null? lat)  (col '() 0 0))
      ((eq? (car lat) oldL)    (multi-insert-c new oldL oldR (cdr lat) (λ (ls l-count r-count)
                                                                         (col (cons new (cons oldL ls)) (add1 l-count) r-count))))
      ((eq? (car lat) oldR)    (multi-insert-c new oldL oldR (cdr lat) (λ (ls l-count r-count)
                                                                         (col (cons oldR (cons new ls)) l-count (add1 r-count)))))
      (else (multi-insert-c new oldL oldR (cdr lat) (λ (ls l-count r-count)
                                                      (col (cons (car lat) ls) l-count r-count)))))))


(multi-insert-c '*** '-- '++ '(3 4 -- a b ++ k -- ++ e) (λ (ls l-n r-n)
                                                          (println ls)
                                                          (println l-n)
                                                          (println r-n)))

(println "------------------------------")
(define evens-only*
  (λ (lst)
    (cond
      ((null? lst) '())
      ((atom? (car lst))   (cond ((even? (car lst)) (cons (car lst) (evens-only* (cdr lst))))
                                 (else (evens-only* (cdr lst)))))
      (else (cons (evens-only* (car lst)) (evens-only* (cdr lst)))))))

(evens-only* '(2
               3
               (4 (5 (6 7 8 9) 10 11) 1)
               (9 9)
               (3 5 6)))

;;  示范3：  col版：  尾递归 + 闭包
;;  closure中存放：  搜集到的偶数 +  这些偶数的乘积 + 奇数的个数
(define even-strong
  (λ (lst col)
    (cond
      ((null? lst) (col '() 1 0))
      ((atom? (car lst))  (cond ((even? (car lst))  (even-strong (cdr lst) (λ (L prod odd-cnt)
                                                                             (col (cons (car lst) L) (@* prod (car lst)) odd-cnt))))
                                (else (even-strong (cdr lst) (λ (L prod odd-cnt)
                                                               (col L prod (add1 odd-cnt)))))))
      (else (cons (even-strong (car lst) col) (even-strong (cdr lst) col))))))

(even-strong '(1 2 3 4 5) (λ (L prod odd-cnt)
                    (println L)
                    (println prod)
                    (println odd-cnt)))

;;******************************************  chapter-9 ***************************************************************************************************
(define pick
  (λ (i lst)
    (cond
      ((null? lst) '())
      ((zero? (sub1 i)) (car lst))
      (else (pick (sub1 i) (cdr lst))))))

(define looking
  (λ (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (λ (a elem lat)
    (cond
      ((number? elem) (keep-looking a (pick elem lat) lat))
      (else (eq? a elem)))))

(looking 'cav '(6 2 4 cav 5 7 3))
(looking 'cav '(6 2 gr cav 5 7 3))

;;-----------------------------------
;; 测试f 这个函数是否对 '() 能够计算出一个结果 ---（可计算性问题？）
;; 可计算问题 无解 ___ 举一个悖论例子即可  —————— 



(define eternity
  (lambda (x)
    (eternity x)))
;;-----------------------------------
;;  只对于 长度为0 的列表可用  
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

(lambda (l)
 (cond
   ((null? l) 0)
   ;; add1 (length-0 (cdr l))
   (else (add1 ((lambda (l)
                  (cond
                    ((null? l) 0)
                    (else (add1 (eternity (cdr l))))))
                (cdr l))))))

;; 再用 length-0 替换 f ,得到的函数就是 length-1
((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ;; 用 eternity替换g (belta规约），得到的函数为 length-0
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  eternity))


((lambda (mk-length)
  (mk-length mk-length))
 (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1
                ((mk-length mk-length) (cdr l))))))))

;;******************************************  chapter-10 ***************************************************************************************************
;; entry :  有点像map
;;  ( (a b c d)
;;    (1 2 3 4))
(define lookup-entry
  ;; entry-f :  当name不存在时，invoke它  == f-name-not-found
  (λ (name entry entry-f)
    (lookup-entry-help name
                       (car entry)
                       (cadr entry)
                       entry-f)))
(define lookup-entry-help
  (λ (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else (lookup-entry-help name (cdr names) (cdr values) entry-f)))))

(lookup-entry 'a '((a b c d)
                   (13 2 3 4)) println)

(define lookup-table
  (λ (name table table-f)
    (cond
      ((null? table) (table-f name))
      ;; 利用entry-f  <--- look up reduced table
      (else (lookup-entry name (car table) (λ (-name)
                                             (lookup-table name (cdr table) table-f)))))))

;;-------------------------------------------------------
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))
























  










              






























 
