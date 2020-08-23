(import srfi-18) ;; threads
(import srfi-1) ;; delete!
(import (chicken time))


;; RESULT :: (void *)
;; READY? :: condition-variable
;; F : (finish x) -> #<unspecified>
(define-record job result ready-cv mutex f)

(define (make-job* f mutex)
  (make-job #f (make-condition-variable) mutex f))

(define (invoke-job job x)
  ;; invoke f
  ((job-f job) (lambda (result)
                 (mutex-lock! (job-mutex job))

                 ;; set result and signal
                 (job-result-set! job result)
                 (condition-variable-signal! (job-ready-cv job))

                 (mutex-unlock! (job-mutex job)))
   x)
  (job-result job))

;; TODO: perhaps clean this a bit (though with the timing and locking,
;; this may be easier said than done)
(define (wait-for-job job timeout)
  (let [(start-ms (current-milliseconds))]
    (mutex-lock! (job-mutex job))
    (if (job-result job)
        ;; its ready
        (let [(result (job-result job))]
          (mutex-unlock! (job-mutex job))
          result)
        ;; otherwise wait for ready-cv signal
        (if (mutex-unlock! (job-mutex job) (job-ready-cv job) timeout)
            ;; try again with the time we have left
            (wait-for-job job (- timeout
                                 (/ (- (current-milliseconds) start-ms)
                                    1000.0)))
            ;; otherwise we ran out of time
            #f))))

(define (make-filter-job pred mutex)
  (make-job* (lambda (finish x)
               (when (pred x)
                 (finish x)))
             mutex))

(define-record job-system mutex roll-mutex roll)

(define (add-job system job)
  (mutex-lock! (job-system-roll-mutex system))
  (job-system-roll-set! system
                        (cons job (job-system-roll system)))
  (mutex-unlock! (job-system-roll-mutex system)))

(define (remove-job system job)
  (mutex-lock! (job-system-roll-mutex system))
  (job-system-roll-set! system (delete! job (job-system-roll system)))
  (mutex-unlock! (job-system-roll-mutex system)))

(define (invoke-jobs system x)
  (mutex-lock! (job-system-roll-mutex system))
  (for-each (cut invoke-job <> x)
            (job-system-roll system))
  (mutex-unlock! (job-system-roll-mutex system)))

(define (wait-for system pred timeout)
  (let [(job (make-filter-job pred (job-system-mutex system)))]
    (add-job system job)
    (let [(ready? (wait-for-job job timeout))]
      ;; remember to remove your jobs
      (remove-job system job)
      (if ready?
          (job-result job)
          #f))))

;; ### TESTING ##################################################################

;; (define system (make-job-system (make-mutex)
;;                                 (make-mutex)
;;                                 '()))
;; 
;; (define (generate-things)
;;   (let loop [(x 1)]
;;     (print "Generator is at " x)
;;     (invoke-jobs system x)
;;     (thread-sleep! 5)
;;     (loop (add1 x))))
;; 
;; (define gen-thread)
;; (define (start-gen)
;;   (set! gen-thread (make-thread generate-things))
;;   (thread-start! gen-thread))
