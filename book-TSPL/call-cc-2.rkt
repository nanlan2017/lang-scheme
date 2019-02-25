#lang racket
(define cont #f)
(define i 0)
(begin
  (display " hello ")
  (display (+ 1 (call/cc (λ (label-1)
                           (set! cont label-1)
                           (+ 2 (label-1 3))))))
  (display " world")
  (set! i (+ 1 i)))

;;  ▇▇▇▇▇▇▇▇▇▇▇▇这个 snapshot  是只读的
;;     ▇▇▇▇▇▇▇▇▇▇不存在说 这个 continuation “拿来 执行两步， 再停下” —————— 你每次用这个contination，都会完整调用它、且调用完这个continuation它自己还是在那儿！  （所以，更像是 mark a label)----当然了，闭包还 保存着 那个点的 已有的运算环境状态

;;******************************************************************

   ;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
(define (generate-one-element-at-a-time lst)

  ;; Hand the next item from a-list to "return" or an end-of-list marker
  (define (control-state return)
    (for-each 
     (λ (element)
       ;; 【▇▇ snapshot-2 : "resume-here"】
       (set! return (call/cc (λ (resume-here)
                               ;; Grab the current continuation
                               (set! control-state resume-here)   ;; 【▇▇ "control-state"】
                               (return element)))))
     lst)
    (return 'you-fell-off-the-end))
  
  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time
  (define (generator)
    ;; 【▇▇ snapshot-1 : "return"】
    (call/cc control-state))

  ;; Return the generator 
  generator)

(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))

(generate-digit) ;; 0
(generate-digit) ;; 1
(generate-digit) ;; 2
(generate-digit) ;; you-fell-off-the-end