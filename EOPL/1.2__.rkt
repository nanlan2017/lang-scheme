#lang racket


(define (occurs-free? var exp)
  (cond
    ;;  'x  'x
    [(symbol? exp) (eqv? var exp)]
    ;; 
    [(eqv? (car exp) 'lambda)
     (and (not (eqv? var (car