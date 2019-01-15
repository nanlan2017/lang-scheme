(module scheduler (lib "eopl.ss" "eopl")

  (require "0-debug.scm")
  (require "1-queues.scm")
  (require "1-data-structures.scm")
  
  (provide (all-defined-out))
  ; ================================================================ 
  ;  the state ： components of the scheduler state:  
  (define %ready-queue   'uninitialized)                    
  
  (define %MaxTimeSlice    'uninitialized)
  (define %time-remaining    'uninitialized)

  (define %FinalAnswer  'uninitialized)

  (define %current-thread-id 'uninitialized)
  (define %list-of-threads 'uninitialized)
  ; ---------------------------
  ; initialize-scheduler! : Int -> ()
  (define (initialize-scheduler! ticks th)
    (set! %ready-queue (empty-queue))
    (set! %FinalAnswer 'uninitialized)
      
    (set! %MaxTimeSlice ticks)
    (set! %time-remaining %MaxTimeSlice)

    (set! %current-thread-id (thread->id th))
    (set! %list-of-threads (list th)))
  
  ; place-on-ready-queue! : Thread -> Unspecified
  (define (place-on-ready-queue! th)
    (set! %ready-queue (enqueue %ready-queue th)))

  ; time-expired? : () -> Bool 
  (define (time-expired?)
    (zero? %time-remaining))

  ; decrement-timer! : () -> Unspecified
  (define (decrement-timer!)
    (when (@debug) (eopl:printf "--------------- %time-remaining : ~s~n" (- %time-remaining 1)))
    (set! %time-remaining (- %time-remaining 1)))

  ; set-final-answer! : ExpVal -> Unspecified 
  (define (set-final-answer! val)
    (set! %FinalAnswer val))

  (define (set-list-of-threads! val)
    (set! %list-of-threads val))
  ; ------------------------------------------------------------------
  ; run-next-thread : () -> FinalAnswer  
  (define (run-next-thread)
    (when (@debug) (eopl:printf "   >>>>>>>>>>>>>> run-next-thread..~n"))
    (if (empty? %ready-queue)
        %FinalAnswer
        (dequeue %ready-queue (lambda (first-ready-thread other-ready-threads)
                                (set! %ready-queue other-ready-threads)
                                (set! %time-remaining %MaxTimeSlice) 
                                (cases Thread first-ready-thread
                                  ($a-thread (id p)
                                             (set! %current-thread-id id)
                                             (p)))
                                ))))
  )
