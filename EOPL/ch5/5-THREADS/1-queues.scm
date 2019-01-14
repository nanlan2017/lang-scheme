(module queues (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  ;; We maintain the queue by adding to the end & dequeuing from the front. 
  (define (empty-queue) '())

  (define empty? null?)

  (define (enqueue q val)
    (append q (list val)))

  (define (dequeue q f)
    (f (car q) (cdr q)))

  )
