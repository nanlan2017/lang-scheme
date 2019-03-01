(module fact-trampolined (lib "eopl.ss" "eopl")

  ; cont的 procedural-presentation : 表现为 apply-cont 下 对 val 的响应。
  (define (end-cont)
    (lambda (val) val))

  (define (fact1-cont n saved-cont)
    (lambda (val) (apply saved-cont (* n val))))

  (define (apply cont val)
    (cont val))

  ;;----------------------------------------------------------------

  (define (fact n)
    (fact/k n (lambda (val) val)))   ; (fact/k n (end-cont))


  (define (fact/k n cont)
    (if (zero? n)
        (cont 1)
        (fact/k (- n 1) (lambda (val) (cont (* n val))))))
  )