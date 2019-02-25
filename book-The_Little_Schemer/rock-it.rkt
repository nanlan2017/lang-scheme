#lang slideshow
;; all of the code that you put in the definitions window is inside a module.
;; Furthermore, the module initially imports everything from the module designated by slideshow,
;; which exports picture-making functions as well as more commonly used functions such as list and map.
(require pict/flash)

(filled-flash 40 30)

(define c (circle 10))
(define r (rectangle 10 20))

(define (square n)
  ; A semi-colon starts a line comment.
  ; The expression below is the function body.
  (filled-rectangle n n))

;; test
(square 30)

;; 定义一个函数
(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(four (circle 20))

(define (checker p1 p2)
  ;; let 
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(checker (colorize (square 10) "red")
         (colorize (square 10) "black"))

(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))

(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))
;;------------------------------------------------------------------
;;; ‘(a b c) 理解成  (list 'a 'b 'c)  --->   '(a b (c d)) 理解成 (list 'a 'b (list 'c 'd))  -------->  不是 (list a b c) <- 这会报错的！
;;;  (list "sa" "sb" "sc“）  可用quote 来写吗？？
;;;  ' 是列表的 字面量写法吗 ？？？？                             ( T )
;;;   我本以为 quote 一个 list会让 列表里的 所有元素 变 symbol ??  ( X )
;;;  ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 不是，是identifier会变成Symbol 
;;;  ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 下面的q1这个list中，非标识符的（string字面值、数字字面值）会变为symbol 。 （即使a已经define，这里也会变为symbol)
(define a "s-a")
(define q1 '(a "s2" b 4))
(car q1)             ; 'a
(symbol? (car q1))   ; #t
(cadr q1)            ; "s2"
(symbol? (cadr q1))  ; #f

(define q2 `(,a "s2" b 4))  ;; ▇▇▇▇▇▇▇▇▇▇▇▇ 此时a由identifer 替换为其值
;; === '("s-a" "s2" b 4)
(define q3 `(,a "s2" ,@(list 3 4)))
;; === '("s-a" "s2" 3 4)



;;     ' *  ===  (quote *)
(quote "fff")  ;; === '"fff  === "fff"
;;------------------------------------------------------------------

(define try
  (λ (x) (2)))

(define (try2 x)
  2)
;;-------------------------------------------------------------
(require slideshow/code)

;;  Macro  : define-syntax ( syntax-rules )
(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))

;; (hc-append 10 (circle 10) (code (circle 10)))
(pict+code (circle 10))

;; 在Common lisp中的写法
;(defmacro pict+code (pic)
;  `(hc-append 10 ,pic (code ,pic)))


;;---------------------------------------------------------
(require racket/class)
(require racket/gui/base)

(define f (new frame% [label "My Art"]
                      [width 300]
                      [height 300]
                      [alignment '(center center)]))

(define path "
 /Users/jiaheng/.rvm/gems/ruby-2.5.1/bin
:/Users/jiaheng/.rvm/gems/ruby-2.5.1@global/bin
:/Users/jiaheng/.rvm/rubies/ruby-2.5.1/bin

:/usr/local/bin
:/usr/bin
:/bin
:/usr/sbin
:/sbin

:/Users/jiaheng/.local/bin
:/Library/Java/JavaVirtualMachines/jdk1.7.0_79.jdk/Contents/Home/bin
:/Applications/Racket")







































