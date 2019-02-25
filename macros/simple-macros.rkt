#lang racket

;; MACRO : swap
(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))


;; ```````````````` test 1
;(define v1 3)
;(define v2 4)
;
;(swap v1 v2)

;; ```````````````` test 2
(let ([tmp 5]
      [other 6])
  (swap tmp other)
  (list tmp other))

;;=============================================================== define-syntax  +  syntax rules ()
(define-syntax rotate
  (syntax-rules ()
    [(rotate a b)    ; pattern 1
     (swap a b)]
    
    [(rotate a b c)  ; pattern 2
     (begin
       (swap a b)
       (swap b c))]))

;```````````````` test 1
(let ([red 1] [green 2] [blue 3])
  (rotate red green)      ; swaps
  (rotate red green blue) ; rotates left
  (list red green blue))
