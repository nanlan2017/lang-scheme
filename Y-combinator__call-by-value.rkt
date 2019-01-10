#lang racket


; 这才是适用于 call by value 的Y combinator 版本 （Scheme为call-by-value）
(define Y
  (λ (f)
    ((λ (x) (f (λ (v) ((x x) v))))
     (λ (x) (f (λ (v) ((x x) v)))))))

(define fact-gen
  (λ (f)
    (λ (n)
      (if (zero? n)
          1
          (* n (f (- n 1)))))))

(define fact (Y fact-gen))

(print (fact 5))

; ======================================================
; 这个只有在Lazy求值下才有效！
(define Y2
  (λ (f)
    ((λ (x) (f (x x)))
     (λ (x) (f (x x))))))

(define fact2 (Y2 fact-gen))

;(print (fact2 5))