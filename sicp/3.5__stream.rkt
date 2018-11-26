#lang racket

;; delay (暂不求值此表达式) | force  （立即求值该被delay的表达式）
(define (s-cons a b)
  (cons a (delay b)))

(define (s-car s)
  (car s))

(define (s-cdr s)
  (force (cdr s)))


(define the-empty-stream '())

(define (s-null? s)
  (equal? s the-empty-stream))

;; stream-cons
;; stream-car, stream-cdr
;; stream-null?
;; the-empty-stream
;;=============================================================================
(define (s-ref s n)
  (if (= n 0)
      (s-car s)
      (s-ref s (- n 1))))

;; stream-map (就和普通的list的map一样，不过要都换成stream上的 cons/car/null...
(define (s-map proc s)
  (if (s-null? s)
      the-empty-stream
      (s-cons (proc (s-car s)) (s-map proc (s-cdr s)))))

;; for-each
(define (s-foreach proc s)
  (if (s-null? s)
      'done
      (begin (proc (s-car s))
             (s-foreach proc (s-cdr s)))))

;;=============================================================================

;; enumerate ： 改为返回一个lazy的list(尚未全部计算）
(define (s-enumerate low high)
  (if (> low high)
      the-empty-stream
      (s-cons low (s-enumerate (+ low 1) high))))

(define (s-filter predict s)
  (cond
    [(s-null? s)
     the-empty-stream]
    [(predict (s-car s))
     (s-cons (s-car s) (s-filter predict (s-cdr s)))]
    [else
     (s-filter predict (s-cdr s))]))

;;=============================================================================
;; 测试： 利用 filter 等找出 10000 - 1000000 之间的第二个素数
;; (n-10000) / 100 > 5   --->  10500, 10200, 10300 ...
(define (pred1 n)
  (> (/ (- n 10000) 100) 5))

;; 原始版本
(define (enumerate low high)
  (if (> low high)
      '()
      (cons low (enumerate (+ low 1) high))))

(cadr (filter pred1  (enumerate 10000 1000000)))
(time (cadr (filter pred1  (enumerate 10000 1000000))))

;(s-car (s-cdr (s-filter pred1 (s-enumerate 10000 1000000))))
;(time (s-car (s-cdr (s-filter pred1 (s-enumerate 10000 1000000)))))

;;=============================================================================
(define ($delay expr)
  (λ () expr))

(define ($force delay-expr)
  (delay-expr))

(define v1 (delay (+ 2 4)))

(define v2 ($delay (+ 2 4)))


;> v1
;#<promise:v1>
;> (force v1)
;6
;> v2
;#<procedure:...p/3.5-stream.rkt:79:2>
;> ($force v2)
;6

;; 仍然是包装一个procedure，但还会记录其是否已求值
(define (memo-history proc)
  (let ([already-run? #f]
        [result #f])
    (λ ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

;; 
(define ($$delay expr)
  (memo-history (λ () expr)))








