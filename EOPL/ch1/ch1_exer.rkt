#lang racket


;;=========================================================================== Exer_1.36
(define (g p ps)
  (let [(modified-ps (map (λ (pair) (list (+ 1 (car pair)) (cadr pair)))
                          ps))]
    (cons p modified-ps)))


(define number-elements
  (lambda (lst)
    (if (null? lst)
        '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(number-elements '(a b c d e))
;;===========================================================================

