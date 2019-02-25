#lang eopl
(provide (all-defined-out))
;**************************************

(define (print-module-a)
  (eopl:printf "Here is module-a"))

(define-datatype Expr Expr?
  (val-exp
   (n number?)))