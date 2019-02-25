#lang racket

#|
--before--                    (..exprs..)   --after
--before-- (call/cc (λ (cont) (..exprs..))) --after--
--before-- (cont --->  f                  ) --after--   // call f with current continuation  （将cont作为参数 提供给 lambda）

         cont v  ===>      --before--v--after--

cont相当于后续的所有计算，即 --before --after
而 call-with-current-continuation ， 上述cont就捕捉的是 current-continuation (当前这一步的continuation-后续计算)

▇▇▇▇▇▇▇▇它使得 一个步骤的continuation对于该计算步骤 可见。（即 cont对于 ...exprs...可见）
本来应该是 ...exprs...计算出一个值，然后作为cont的输入参数进行自然调用。
现在你可以在 ...exprs...中 直接 （cont xx）， 即是说：xx当然替代的是 (...exprs...) 本该计算出来的值， 所以它相当于说： 让(...exprs...) 提前完成计算、值就是xx，从而进入后续计算
|#

(call/cc (λ (k) (k 42)))
(call/cc (lambda (k) (+ (k 42) 1729)))

(let ((cont #f))
  (call/cc (λ (k) (set! cont k)))
  (cont #f))


;;  使得某一个计算可以 挂起。 并可以把该 “挂起的剩余计算”  当做数据结果 （Continuation) 进行传递。

--- 首先，要会捕捉一个 continuation （通过call/cc)
---  (call/cc  f-need-cont)  ====  (f-need-cont 'current-continuation)  // 相当于一个调用
--- 继续一个“挂起的计算” :    (cont v) 会发生什么？  -------- 应该是进行 把 v替换为 原来 (call/cc 的结果值)那里 进行继续


(if (null? x) (quote ()) (cdr x))

(null? x)
(quote ())
(cdr x)


(define cont #f)
(define j 0)
(define (test)
  (let ((i 0))
    ; call/cc calls its first function argument, passing
    ; a continuation variable▇▇▇▇▇▇▇ representing this point in
    ; the program as the argument to that function.
    ;
    ; In this case, the function argument assigns that
    ; continuation to the variable the-continuation.
    ;
    (call/cc (lambda (k) (set! cont k)))  
    ; -------------  此处就是 k 这个continuation 对应的那个 “点”
    ; ▇▇▇▇▇▇▇▇ The next time the-continuation is called, we start here.
    ; --------------  就像一段代码执行到某行 得到一个值， 我们暂时 把 这个位置点给记住 （后续想恢复到这里继续执行时， 用  cont v ，  则回到这个“点”,并且以v作为那一行 的计算结果，继续进行计算
    (set! i (+ i 1))
    (set! j (+ j 1))
    i))

;;**********************************************************
;;  x 是一个continuation:  将v  作为函数、用 (λ (ignore) "hi") 作用参数来调用这个函数
(let ([x (call/cc
          (λ (k) k))])
  (x (λ (ignore) "hi")))

;;  x ===  (λ (p)
;;            (p (λ (ignore) "hi"))
;;-->   ((λ (ignore) "hi") (λ (ignore) "hi"))  --->  alpha替换，得到 "hi"


相当于：
(let ([x (λ (ignore) "hi")])
  (x (λ (ignore) "hi")))  ;;  ▇▇▇▇▇▇▇▇cont p ： 用p替换  (call/cc ..) 这一句  ------> 每个cont都对应在代码的某个点（某个值的计算）， 用cont p 的p 来替换掉这个值继续执行即可
;;**********************************************************
(( (call/cc (λ (k) k))
   (λ (x) x))
 "HEY!")
;--->  
(( (λ (x) x)   ;;  k xx : 则把 xx 代换原来的 (call/cc ..)
   (λ (x) x))
 "HEY!")
;-->
((λ (x) x) "HEY!")
这样就用f 来替换原来的call/cc整句， 
;;**********************************************************
(define retry #f)

(define factorial
  (λ (x)
    (if (= x 0)
        ;;  mark the following expr as  cont-expr--> retry 
        (call/cc (λ (k)
                   (set! retry k)
                   1))
        (* x (factorial (- x 1))))))
----->  
> (factorial 5)
120
> retry          ;; retry ：  5x4x3x2* ? 
#<continuation>
> (retry 10)
1200

;;****************************************************************************************************************************
;;  实现 return
(define (f return)
  (return 2)
  3)
(display (f (λ (x) x))) ; displays 3
(display (call/cc f)) ; displays 2
;;************************************************
;; 实现 break
(define product
  (λ (ls)
    (call/cc
      (λ (break)
        (let f ([ls ls])
          (cond
            [(null? ls) 1]
            [(= (car ls) 0) (break 0)]
            [else (* (car ls) (f (cdr ls)))]))))))
;;************************************************
;;; A naive queue for thread scheduling.
;;; It holds a list of continuations "waiting to run".
(define *queue* '())
(define (empty-queue?)
     (null? *queue*))
 
(define (enqueue x)
     (set! *queue* (append *queue* (list x))))

(define (dequeue)
     (let ((x (car *queue*)))
       (set! *queue* (cdr *queue*))
       x))

;; 当前的表达式 e1 +  后续的计算 e2


current-v :  (+ 1 2)
current-cont: (λ (x) (* 3 x))
(* 3 (+ 1 2))
--->  (*3 (call/cc (λ (cont) (+1 2))))
--->  ((λ (cont) (cont (+ 1 2)) (λ (x) (* 3 x)))

;;; This starts a new thread running (proc).
(define (fork proc)
     ;;  ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇要理解 continuation 对象，一定要记清楚 它对应的 是哪个“点”-----（一个continuaiton对象必定对应一个 点）
     ;;  记清楚它的点后，它就和一个正常的 函数对象没有什么 区别。
     ;;  特殊就特殊在使用：   (cc expr)  会相当于用  expr 替换它所对应的那个点的 (call/cc ...) ，
     ;;  ——————————▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ contination就相当于 一个 程序调用栈 的snapshot (包含着计算状态)
     (call/cc
      (λ (k)
        (enqueue k)
        (proc))))

;;; This yields the processor to another thread, if there is one.
(define (yield)
     (call/cc
      (λ (k)
        (enqueue k)
        ((dequeue)))))

;;; This terminates the current thread, or the entire program
;;; if there are no other threads left.
(define (thread-exit)
     (if (empty-queue?)
         (exit)
         ((dequeue))))

(define (do-stuff-n-print str)
     (λ ()
       (let loop ((n 0))
         (when (< n 10)
         (display (format "~A ~A\n" str n))
         (yield)
         (loop (+ 1 n))))))
 
;;; Create two threads, and start them running.
(fork (do-stuff-n-print "This is AAA"))
(fork (do-stuff-n-print "Hello from BBB"))
(thread-exit)
;;*****************************************************************

(define lwp-list '())
(define lwp
  (lambda (thunk)
    (set! lwp-list (append lwp-list (list thunk))))) 

(define start
  (lambda ()
    (let ([p (car lwp-list)])
      (set! lwp-list (cdr lwp-list))
      (p))))

(define pause
  (lambda ()
    (call/cc
      (lambda (k)
        (lwp (lambda () (k #f)))
        (start)))))

(lwp (lambda () (let f () (pause) (display "h") (f))))
(lwp (lambda () (let f () (pause) (display "e") (f))))
(lwp (lambda () (let f () (pause) (display "y") (f))))
(lwp (lambda () (let f () (pause) (display "!") (f))))
(lwp (lambda () (let f () (pause) (newline) (f))))
(start)
;;*****************************************************************
(let* ((yin
         ((lambda (cc)
            (display #\@) cc)
          (call/cc
           (lambda (c) c))))
       (yang
         ((lambda (cc)
            (display #\*) cc)
          (call/cc
           (lambda (c) c)))))
    (yin yang))


(define r #f)
(begin  (display " hello ")
        (display  (+ 1
                      (call/cc   (lambda (k)
                                          (set! r k)
                                          (+ 2 (k 3))))))
        (display " world ")
        (newline))

