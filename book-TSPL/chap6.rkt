#lang racket


(require rnrs/records/syntactic-6)
(require rnrs)

(define-record-type point
  (fields (mutable x) (immutable y)))

#|
(define p (make-point 36 -17))
(point? p)
(point? '(cons 36 -17))
(point-x p)
(point-y p)
(point-x-set! p 26)
|#
;; ***********************************************************
(define rtd/parent
  (make-record-type-descriptor 'parent #f #f #f #f
    '#((mutable x)))) 

(record-type-descriptor? rtd/parent) 
(define parent? (record-predicate rtd/parent))
(define parent-x (record-accessor rtd/parent 0))
(define set-parent-x! (record-mutator rtd/parent 0)) 

(define rtd/child
  (make-record-type-descriptor 'child rtd/parent #f #f #f
    '#((mutable x) (immutable y)))) 

(define child? (record-predicate rtd/child))
(define child-x (record-accessor rtd/child 0))
(define set-child-x! (record-mutator rtd/child 0))
(define child-y (record-accessor rtd/child 1)) 

(record-mutator rtd/child 1)

(define rcd/parent
  (make-record-constructor-descriptor rtd/parent #f
    (lambda (new) (lambda (x) (new (* x x)))))) 

(record-type-descriptor? rcd/parent) 

(define make-parent (record-constructor rcd/parent)) 

(define p (make-parent 10))
(parent? p) <graphic> #t
(parent-x p) <graphic> 100
(set-parent-x! p 150)
(parent-x p) <graphic> 150 

(define rcd/child
  (make-record-constructor-descriptor rtd/child rcd/parent
    (lambda (pargs->new)
      (lambda (x y)
        ((pargs->new x) (+ x 5) y))))) 

(define make-child (record-constructor rcd/child))
(define c (make-child 10 'cc))
(parent? c) 
(child? c) 
(child? p) 

(parent-x c) 
(child-x c) 
(child-y c)

(child-x p)