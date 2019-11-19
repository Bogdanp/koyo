#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         component
         db
         mzlib/os
         racket/async-channel
         racket/class
         racket/contract
         racket/list
         racket/match
         racket/place
         racket/string
         "broker.rkt"
         "job.rkt"
         "registry.rkt"
         "serialize.rkt")

(provide
 current-system
 start-worker
 start-worker-place)

(define-logger job-worker)

(define/contract current-system
  (parameter/c (or/c false/c system?))
  (make-parameter #f))

(define-syntax (start-worker stx)
  (syntax-parse stx
    [(_ system-id:id
        (~alt
         (~optional (~seq #:queue queue:expr) #:name "#:queue parameter")
         (~optional (~seq #:broker-id broker-id:expr) #:name "#:broker-id parameter")
         (~optional (~seq #:pool-size pool-size:expr) #:name "#:pool-size parameter")) ...)
     #:with source (datum->syntax stx (syntax-source stx))
     #'(start-worker* #:source source
                      #:system system-id
                      #:system-id 'system-id
                      #:broker-id (~? broker-id 'broker)
                      #:queue (~? queue "default")
                      #:pool-size (~? pool-size (processor-count)))]))

(define/contract (start-worker* #:source source
                                #:system sys
                                #:system-id [system-id 'prod-system]
                                #:broker-id [broker-id 'broker]
                                #:pool-size [pool-size (processor-count)]
                                #:queue [queue "default"])
  (->* (#:source path-string?
        #:system system?)
       (#:system-id symbol?
        #:broker-id symbol?
        #:pool-size exact-positive-integer?
        #:queue non-empty-string?)
       (-> void?))
  (parameterize ([current-custodian (make-custodian)])
    (system-start sys)
    (define broker (system-ref sys broker-id))
    (define reactor (make-reactor source system-id broker queue pool-size))
    (lambda _
      (reactor-stop reactor)
      (system-stop sys))))

(struct reactor (ch thd)
  #:transparent)

(define (make-reactor source system-id broker queue pool-size)
  (log-job-worker-debug "starting reactor...")
  (define id (broker-register-worker! broker (getpid) (gethostname)))
  (define ch (make-async-channel))
  (define pool (make-worker-pool pool-size source system-id))
  (define listener (make-listener broker id queue))
  (define thd
    (thread
     (lambda _
       (let loop ([pool pool]
                  [idle pool]
                  [busy null])
         (sync
          (handle-evt
           (listener-jobs-evt listener (length idle))
           (lambda (jobs)
             (loop pool
                   (drop idle (length jobs))
                   (for/fold ([busy busy])
                             ([p (in-list idle)]
                              [j (in-list jobs)])
                     (place-channel-put p (list 'exec j))
                     (cons p busy)))))

          (handle-evt
           ch
           (match-lambda
             [(list 'stop)
              (worker-pool-stop pool)
              (listener-stop listener)
              (loop pool idle busy)]))

          (apply
           choice-evt
           (for/list ([p (in-list pool)])
             (handle-evt
              p
              (match-lambda
                [(list 'stopped)
                 (log-job-worker-debug "worker-place ~.s stopped" p)
                 (define pool* (remq p pool))
                 (cond
                   [(null? pool*)
                    (log-job-worker-debug "all worker places have been stopped")
                    (broker-unregister-worker! broker id)]

                   [else
                    (loop pool* idle busy)])]

                [(list 'done (vector id queue job arguments attempts))
                 (log-job-worker-debug "job ~a done" id)
                 (broker-mark-done! broker id)
                 (loop pool (cons p idle) (remq p busy))]

                ;; TODO: retry-when style logic.
                [(list 'error (vector id queue job arguments attempts) message)
                 (log-job-worker-error "job ~a failed: ~a" id message)
                 (broker-mark-failed! broker id)
                 (loop pool (cons p idle) (remq p busy))])))))))))

  (reactor ch thd))

(define (reactor-stop reactor)
  (log-job-worker-debug "stopping reactor...")
  (async-channel-put (reactor-ch reactor) '(stop))
  (thread-wait (reactor-thd reactor)))

(struct listener (broker id queue [conn #:mutable])
  #:transparent)

(define (make-listener broker id queue)
  (define conn (broker-borrow-connection broker))
  (query-exec conn "LISTEN koyo_jobs")
  (listener broker id queue conn))

(define (listener-stop listener)
  (log-job-worker-debug "stopping listener...")
  (define broker (listener-broker listener))
  (define conn (listener-conn listener))
  (query-exec conn "UNLISTEN *")
  (broker-release-connection broker conn)
  (set-listener-conn! listener #f))

(define (listener-jobs-evt the-listener limit)
  (match the-listener
    [(listener broker id queue #f) never-evt]
    [(listener broker id queue conn)
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
                     (* (random 30 300) 1000)))
       (lambda _
         (begin0 null
           (broker-perform-maintenance! broker id)))))]))

(define (make-worker-pool size source system-id)
  (define pool
    (for/list ([_ (in-range size)])
      (define p (dynamic-place 'koyo/job/worker 'start-worker-place))
      (begin0 p
        (place-channel-put p (list 'load source system-id)))))

  (begin0 pool
    (for ([i (in-naturals 1)]
          [p (in-list pool)])
      (match (place-channel-get p)
        [(list 'ok)
         (log-job-worker-debug "worker place ~a ready" i)]

        [(list 'error message)
         (error 'make-worker-pool "failed to load source ~.s: ~a" source message)]))))

(define (worker-pool-stop pool)
  (log-job-worker-debug "stopping worker pool...")
  (for ([p (in-list pool)])
    (place-channel-put p '(stop))))

(define (start-worker-place ch)
  (let loop ()
    (match (sync ch)
      [(list 'stop)
       (place-channel-put ch '(stopped))]

      [(list 'load source system-id)
       ;; Worker places dynamically require the source module that
       ;; started the worker so that jobs get added to the registry.
       ;; Additionaly, they parameterize current-system so that jobs can
       ;; grab components off of it.
       (current-system
        (dynamic-require
         source
         system-id
         (lambda _
           (place-channel-put ch (list 'error "failed to load system")))))

       (place-channel-put ch '(ok))
       (loop)]

      [(list 'exec (and (vector id queue job arguments attempts) data))
       (with-handlers ([(lambda _ #t)
                        (lambda (e)
                          (place-channel-put ch (list 'error data (exn-message e))))])
         (define proc (job-proc (lookup (format "~a.~a" queue job))))
         (apply keyword-apply proc (deserialize arguments))
         (place-channel-put ch (list 'done data)))

       (loop)])))
