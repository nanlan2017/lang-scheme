#lang racket
(require racket/trace)


;; cont是一个函数；其只通过apply-cont来表示
(define (apply-cont cont val)
  (cont val))

;apply-cont (multcont 4 cont) val
;= (multcont 4 cont) val
;= (λ (v) (cont (* 4 v))) val
;= cont (* 4 val)
(define (multcont n cont)
  (λ (val)
    (cont (* n val))))

; 作为初始参数
(define (end-cont)
  (λ (val) (begin
             (println "End of calculation....")
             val)))


;; 语义：  【cont】 (fact n) == 【cont (* 4)】(fact (n-1))
(define (fact n cont)
  (if (= 0 n)
      (apply-cont cont 1)
      (fact (- n 1) (multcont n cont))))


(trace fact)
(fact 5 (end-cont))  ;; 120

;;=============================================================
(trace-define (raw-fact n)
  (if (= n 0)
      1
      (* n (raw-fact (- n 1)))))

(raw-fact 5)

;;-----------------------------
;(trace-define (rec-fact n)
;  (letrec ([inner-rec (λ (c res)
;                        (if (= n 0)
;                            res
;                            (inner-rec (- c 1) (* c res))))])
;    (inner-rec n 1)))

(trace-define (rec-fact n)
  (inner-rec n 1))            

(trace-define (inner-rec n res)
  (if (= n 0)
      res
      (inner-rec (- n 1) (* n res)))) 

(rec-fact 5)






















