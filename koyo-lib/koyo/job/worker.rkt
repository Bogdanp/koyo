#lang racket/base

(require component
         db
         gregor
         mzlib/os
         racket/async-channel
         racket/class
         racket/contract
         racket/list
         racket/match
         racket/string
         "../database.rkt"
         "broker.rkt"
         "job.rkt"
         "registry.rkt"
         "serialize.rkt")

;; worker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-worker-factory
 worker?)

(define-logger worker)

(struct worker (broker queue pool-size reactor)
  #:transparent
  #:methods gen:component
  [(define (component-start w)
     (define broker (worker-broker w))
     (define queue (worker-queue w))
     (define pool-size (worker-pool-size w))
     (struct-copy worker w [reactor (make-reactor broker queue pool-size)]))

   (define (component-stop w)
     (reactor-stop (worker-reactor w))
     (struct-copy worker w [reactor #f]))])

(define/contract ((make-worker-factory #:queue [queue "default"]
                                       #:pool-size [pool-size 8]) broker)
  (->* ()
       (#:queue non-empty-string?
        #:pool-size exact-positive-integer?)
       (-> broker? worker?))
  (worker broker queue pool-size #f))


;; reactor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-reactor broker queue pool-size)
  (log-worker-debug "starting reactor...")
  (define id (broker-register-worker! broker (getpid) (gethostname)))
  (define ch (make-async-channel pool-size))
  (define pool (make-worker-pool pool-size ch))
  (define listener (make-listener broker id queue))
  (thread
   (lambda _
     (let loop ([idle pool]
                [busy null])
       (sync
        (handle-evt
         (thread-receive-evt)
         (lambda (_)
           (match (thread-receive)
             [(list 'stop)
              (listener-stop listener)
              (worker-pool-stop pool)
              (broker-unregister-worker! broker id)])))

        (handle-evt
         (listener-jobs-evt listener (length idle))
         (lambda (jobs)
           (loop (drop idle (length jobs))
                 (for/fold ([busy busy])
                           ([p (in-list idle)]
                            [j (in-list jobs)])
                   (thread-send p (list 'exec j))
                   (cons p busy)))))

        (handle-evt
         ch
         (match-lambda
           [(list 'done t (vector id queue job-id arguments attempts))
            (log-worker-debug "job ~a done" id)
            (broker-mark-done! broker id)
            (loop (cons t idle) (remq t busy))]

           [(list 'retry t (vector id queue job-id arguments attempts) reason delay-ms)
            (log-worker-debug "job ~a requested retry with delay ~.s" id delay-ms)
            (broker-mark-for-retry! broker id (+milliseconds (now/moment) delay-ms))
            (loop (cons t idle) (remq t busy))]

           [(list 'fail t (vector id queue job-id arguments attempts) message)
            (log-worker-warning "job ~a failed: ~a" id message)
            (broker-mark-failed! broker id)
            (loop (cons t idle) (remq t busy))])))))))

(define (reactor-stop r)
  (log-worker-debug "stopping reactor...")
  (thread-send r '(stop))
  (thread-wait r))


;; listener ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct listener (broker id queue [conn #:mutable])
  #:transparent)

(define (make-listener broker id queue)
  (define conn (broker-borrow-connection broker))
  (query-exec conn "LISTEN koyo_jobs")
  (listener broker id queue conn))

(define (listener-stop l)
  (log-worker-debug "stopping listener...")
  (define broker (listener-broker l))
  (define conn (listener-conn l))
  (set-listener-conn! l #f)
  (query-exec conn "UNLISTEN *")
  (broker-release-connection broker conn))

(define (listener-jobs-evt l limit)
  (match l
    [(listener broker id queue #f) never-evt]
    [(listener broker id queue conn)
     (parameterize ([current-database-connection conn])
       (choice-evt
        (cond
          [(zero? limit) never-evt]
          [else
           (guard-evt
            (lambda _
              (match (broker-dequeue! broker id queue limit)
                [(list) never-evt]
                [jobs (handle-evt always-evt (lambda _ jobs))])))])

        (handle-evt
         (send+ conn
                (get-base)
                (async-message-evt))
         (lambda (ready?)
           (cond
             [ready? (broker-dequeue! broker id queue limit)]
             [else null])))

        (handle-evt
         (alarm-evt (+ (current-inexact-milliseconds)
                       (* (random 15 60) 1000)))
         (lambda _
           (begin0 null
             (log-worker-debug "performing maintenance...")
             (broker-perform-maintenance! broker id))))))]))


;; worker threads ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A worker pool is just a list of worker threads.  Worker threads
;; accept messages via their built-in mailboxes and reply via a
;; response channel that is passed in.

(define (make-worker-pool size ch)
  (for/list ([id (in-range size)])
    (make-worker-thread id ch)))

(define (worker-pool-stop pool)
  (for ([thd (in-list pool)])
    (thread-send thd '(stop)))

  (sync
   (handle-evt
    (alarm-evt (+ (current-inexact-milliseconds) 60000))
    (lambda _
      (for-each kill-thread pool)))
   (thread
    (lambda ()
      (for-each sync pool)))))

(define (make-worker-thread id ch)
  (define-syntax-rule (send id arg ...)
    (async-channel-put ch (list 'id arg ...)))

  (define thd
    (thread
     (lambda ()
       (log-worker-debug "worker thread ~a started..." id)
       (let loop ()
         (match (thread-receive)
           [(list 'stop)
            (log-worker-debug "stopping worker thread ~a..." id)]

           [(list 'exec (and (vector id queue job-id arguments attempts) job))
            (log-worker-debug "processing job ~.s..." job)
            (with-handlers ([exn:job:retry?
                             (lambda (e)
                               (send retry thd job (exn-message e) (exn:job:retry-delay-ms e)))]

                            [exn:fail?
                             (lambda (e)
                               (send fail thd job (exn-message e)))])
              (define proc (job-proc (lookup (format "~a.~a" queue job-id))))
              (apply keyword-apply proc (deserialize arguments))
              (send done thd job))
            (loop)])))))

  thd)
