(module semaphores (lib "eopl.ss" "eopl")

  (require "0-store.scm")                    ; for store ops
  (require "1-data-structures.scm")          ; for lock, a-lock
  (require "2-scheduler.scm")                ; for os calls
  (require "1-queues.scm")

  (provide (all-defined-out))
  ; implements binary semaphores (mutexes).
  ; =========================================================================

  (define Option@-instrument-mutexes (make-parameter #f))

  ; new-mutex () -> Mutex
  (define (new-mutex)
    ($a-mutex (newref #f) (newref '())))               

  ; ------------------------------------------------------------------------- wait queue, initially empty
  ;; waits for mutex to be open, then closes it.
  ;; Page: 190
  ;; wait-for-mutex : Mutex * Thread -> FinalAnswer
  (define (wait-for-mutex m trd)
    (cases Mutex m
      ($a-mutex (ref-to-closed? ref-to-wait-queue)
                (cond
                  ((deref ref-to-closed?) (setref! ref-to-wait-queue (enqueue (deref ref-to-wait-queue) trd))
                                          (run-next-thread))
                  (else (setref! ref-to-closed? #t)
                        (trd))))))

  ;; signal-mutex : Mutex * Thread -> FinalAnswer
  ;; Page 190
  (define (signal-mutex m th)
    (cases Mutex m
      ($a-mutex (ref-to-closed? ref-to-wait-queue)
                (let ((closed? (deref ref-to-closed?))
                      (wait-queue (deref ref-to-wait-queue)))
                  (when closed?
                    (if (empty? wait-queue)
                        (setref! ref-to-closed? #f)
                        (dequeue wait-queue (lambda (first-waiting-th other-waiting-ths)
                                              (place-on-ready-queue! first-waiting-th)
                                              (setref! ref-to-wait-queue other-waiting-ths)))))
                  (th)))))

  )
