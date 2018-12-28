(module queues (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  ;; We maintain the queue by adding to the end and dequeuing from the front. 
  (define empty-queue
    (lambda ()
      '()))

  (define empty? null?)

  (define enqueue
    (lambda (q val)
      (append q (list val))))

  (define dequeue
    (lambda (q f)
      (f (car q) (cdr q))))

  )
