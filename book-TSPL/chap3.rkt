#lang racket

(define-syntax or ; incorrect!
  (syntax-rules ()
    [(_) #f]
    [(_ e1 e2 ...)
     (let ([t e1])
       (if t t (or e2 ...)))]))


(or 'exp-1 'exp-2 'exp-3)


(((call/cc (λ (k) k)) (λ (x) x)) "HEY!")
(((λ (x) x) (λ (x) x)) "HEY!")
((λ (x) x) "HEY!")
"HEY!"
;;*********************************************
(define lwp-list '())
(define lwp
  (lambda (thunk)
    (set! lwp-list (append lwp-list (list thunk))))) 

;; 从任务池中取出一个任务并执行
(define start
  (lambda ()
    (let ([p (car lwp-list)])
      (set! lwp-list (cdr lwp-list))
      (p))))

(define pause
  (lambda ()
    ;; 捕捉我这个(pause)的continuation k
    ;; 并向任务池的末尾添加一个任务  k #f
    ;; 然后启动start
    (call/cc (lambda (k)
               (lwp (lambda () (k #f)))
               (start)))))


(lwp (lambda ()
       (let f ()
         (pause)  ;; 这里捕捉到的k 是 在递归之中：  
         (display "h")
         (f))))
(lwp (lambda () (let f () (pause) (display "e") (f))))
(lwp (lambda () (let f () (pause) (display "y") (f))))
(lwp (lambda () (let f () (pause) (display "!") (f))))
(lwp (lambda () (let f () (pause) (newline) (f))))
(start)

(let ((k.n (call/cc (lambda (k) (cons k 0)))))
  (let ((k (car k.n)) (n (cdr k.n)))
    (write n)
    (newline)
    (k (cons k (+ n 1)))))
;;*********************************************
;;  It turns out that any program that uses call/cc can be rewritten in CPS without call/cc
(define product
  (lambda (ls k)
    
    (let ([break k])
      (let f ([ls ls] [k k])
        (cond
          [(null? ls) (k 1)]
          [(= (car ls) 0) (break 0)]
          [else (f (cdr ls)
                   (lambda (x)
                     (k (* (car ls) x))))])))))