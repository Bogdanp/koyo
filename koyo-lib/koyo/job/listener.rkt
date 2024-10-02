#lang racket/base

(require db
         racket/class
         racket/match
         "../database.rkt"
         "broker.rkt"
         "logger.rkt")

(provide
 exn:fail:listener?
 exn:fail:listener-original-e
 make-listener
 listener-stop
 listener-jobs-evt)

(struct exn:fail:listener exn:fail (original-e))

(struct listener (broker id queue [conn #:mutable])
  #:transparent)

(define (make-listener broker id queue)
  (define conn (broker-borrow-connection broker))
  (query-exec conn "LISTEN koyo_jobs")
  (listener broker id queue conn))

(define (listener-stop l)
  (log-worker-debug "stopping listener")
  (define broker (listener-broker l))
  (define conn (listener-conn l))
  (set-listener-conn! l #f)
  (with-handlers ([exn:fail? void])
    (query-exec conn "UNLISTEN *"))
  (broker-release-connection broker conn))

(define (listener-jobs-evt l limit)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (raise
                      (exn:fail:listener
                       "listener failed"
                       (current-continuation-marks)
                       e)))])
    (match l
      [(listener broker id queue #f) never-evt]
      [(listener broker id queue conn)
       (parameterize ([current-database-connection conn])
         (choice-evt
          (cond
            [(zero? limit)
             (begin0 never-evt
               (log-worker-debug "no slots for dequeue"))]

            [else
             (choice-evt
              (replace-evt
               (send+ conn
                      (get-base)
                      (async-message-evt))
               (lambda (ready?)
                 (cond
                   [ready?
                    (begin0 (dequeue-evt broker id queue limit)
                      (log-worker-debug "received job notification"))]

                   [else never-evt])))

              (dequeue-evt broker id queue limit))])

          (handle-evt
           (alarm-evt
            (+ (current-inexact-monotonic-milliseconds)
               (* (random 1 15) 1000))
            #t)
           (lambda (_)
             (begin0 null
               (log-worker-debug "performing maintenance...")
               (broker-perform-maintenance! broker id))))))])))

(define (dequeue-evt broker id queue limit)
  (nack-guard-evt
   (lambda (nack)
     (match (broker-dequeue! broker id queue limit)
       [(list)
        (begin0 never-evt
          (log-worker-debug "nothing to dequeue"))]

       [jobs
        (define ids
          (for/list ([j (in-list jobs)])
            (vector-ref j 0)))

        (thread
         (lambda ()
           (sync nack)
           (broker-requeue! broker id ids)
           (log-worker-debug "requeued jobs due to nack: ~.s" ids)))

        (begin0 (wrap-evt always-evt (λ (_) jobs))
          (log-worker-debug "dequeued jobs: ~.s" ids))]))))
