#lang racket/base

(require actor
         data/monocle
         gregor
         racket/match
         racket/os
         racket/promise
         threading
         "broker.rkt"
         "job.rkt"
         "listener.rkt"
         "logger.rkt"
         "registry.rkt"
         "serialize.rkt")

(provide
 reactor
 stop-reactor)

(struct state (id broker idle promises listener custodian middleware stopped?))
(define-struct-lenses state)

(define-actor (reactor broker queue pool-size middleware)
  #:stopped? state-stopped?
  #:state (make-state broker queue pool-size middleware)
  #:event (lambda (st)
            (with-handlers ([exn:fail:listener?
                             (lambda (e)
                               (define original
                                 (exn:fail:listener-original-e e))
                               ((error-display-handler)
                                (exn-message original)
                                original)
                               (state-repair-listener st broker queue))])
              (apply
               choice-evt
               (wrap-evt
                (listener-jobs-evt
                 (state-listener st)
                 (state-idle st))
                (lambda (jobs)
                  (for/fold ([st st])
                            ([j (in-list jobs)])
                    (state-add-job st j))))
               (for/list ([promise (in-list (state-promises st))])
                 (wrap-evt
                  promise
                  (lambda (_void)
                    (~> (lens-update &state-promises st (λ (promises) (remq promise promises)))
                        (lens-update &state-idle _ add1))))))))

  (define (stop-reactor st)
    (log-worker-debug "stopping reactor")
    (listener-stop (state-listener st))
    (define next-st (state-stop st))
    (broker-unregister-worker! broker (state-id st))
    (values next-st (void))))

(define (make-state broker queue pool-size middleware)
  (define id
    (broker-register-worker!
     broker
     (getpid)
     (gethostname)))
  (state
   #;id id
   #;broker broker
   #;idle pool-size
   #;promises null
   #;listener (make-listener broker id queue)
   #;custodian (make-custodian)
   #;middleware middleware
   #;stopped? #f))

;; invariant: idle is positive
(define (state-add-job st j)
  (match-define (state _ broker idle promises _ custodian middleware _) st)
  (match-define (vector id queue name arguments _) j)
  (define promise
    (parameterize ([current-broker broker]
                   [current-custodian custodian])
      (delay/thread
       (define job-thread (current-thread))
       (define job-custodian (make-custodian))
       (thread
        (lambda ()
          (sync (thread-dead-evt job-thread))
          (custodian-shutdown-all job-custodian)))
       (parameterize ([current-custodian job-custodian])
         (with-handlers ([exn:job:retry?
                          (lambda (e)
                            (define reason (exn-message e))
                            (define delay-ms (exn:job:retry-delay-ms e))
                            (log-status j "job retried~n  reason: ~a~n  delay: ~sms" reason delay-ms)
                            (with-handlers ([exn:fail? log-update-error])
                              (broker-mark-for-retry! broker id (+milliseconds (now/moment) delay-ms))))]

                         [exn:fail?
                          (lambda (e)
                            ((error-display-handler) (exn-message e) e)
                            (log-status j "job failed~n  error: ~a" (exn-message e))
                            (with-handlers ([exn:fail? log-update-error])
                              (broker-mark-failed! broker id)))])
           (define proc
             (job-proc
              (lookup (format "~a.~a" queue name))))
           (apply
            keyword-apply
            (middleware proc)
            (deserialize arguments))
           (log-status j "job suceeded")
           (with-handlers ([exn:fail? log-update-error])
             (broker-mark-done! broker id)))))))
  (~> (&state-idle st (sub1 idle))
      (&state-promises (cons promise promises))))

(define (state-repair-listener st broker queue)
  (listener-stop (state-listener st))
  (~> (make-listener broker (state-id st) queue)
      (&state-listener st _)))

(define (state-stop st [timeout 60])
  (define deadline
    (+ (current-inexact-monotonic-milliseconds)
       (* timeout 1000)))
  (let loop ([promises (state-promises st)])
    (unless (null? promises)
      (apply
       sync
       (handle-evt (alarm-evt deadline #t) void)
       (for/list ([promise (in-list promises)])
         (handle-evt
          promise
          (lambda (_)
            (loop (remq promise promises))))))))
  (custodian-shutdown-all
   (state-custodian st))
  (~> (&state-promises st null)
      (&state-stopped? #t)))
