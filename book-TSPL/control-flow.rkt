;; https://www.jianshu.com/p/e860f95cad51
;; <用call/cc合成所有的控制结构>
#lang racket
(require racket/stxparam)

(define-syntax-parameter break (syntax-rules ()))

(define-syntax-parameter continue (syntax-rules ()))

(define-syntax while
  (syntax-rules ()
    [(_ test body ...)
     (call/cc (lambda (k1)
                (let ([t (void)])
                  (begin (call/cc (lambda (k2) (set! t k2)))
                         (syntax-parameterize
                             ([break (syntax-rules ()
                                       [(_) (k1 (void))])]
                              [continue (syntax-rules ()
                                          [(_) (t (void))])])
                           (when (not test) (break))
                           body ... (continue))))))]))

(let ([a 1])
  (while (< a 10)
         (set! a (+ a 1))
         (display a)))

(let ([a 1])
  (while (< a 10)
         (set! a (+ a 1))
         (when (= a 5) (break))
         (display a)))

(let ([a 1])
  (while (< a 10)
         (set! a (+ a 1))
         (when (= a 5) (continue))
         (display a)))

(let ([a 1])
  (while (< a 10)
         (set! a (+ a 1))
         (let ([b 1])
           (while (< b a)
                (display b)
                (display " ")
                (set! b (+ b 1))
                (when (= b 5) (break))
                )
         (display a)
         (display " "))))