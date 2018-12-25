(module utils (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (define (drop lst cnt)
    (let [(size (length lst))]
      (cond
        [(> cnt size) (eopl:error "drop : too much!")]
        [(= cnt size) '()]
        [(= 0 cnt) lst]
        [else (drop (cdr lst) (- cnt 1))])))

  (define (take lst cnt)
    (let [(size (length lst))]
      (cond
        [(> cnt size) (eopl:error "take : too much!")]
        [(= cnt size) lst]
        [(= 0 cnt) '()]
        [else (cons (car lst)
                    (take (cdr lst) (- cnt 1)))])))

  (drop '(1 2 3 4) 2)
  (take '(1 2 3 4) 3)
      
  )
