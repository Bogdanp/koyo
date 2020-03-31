#lang racket/base

(require component
         component/testing
         db
         koyo/database
         koyo/job
         koyo/job/broker
         koyo/job/serialize
         racket/async-channel
         racket/match
         rackunit)

(provide
 worker-system
 job-tests)

(define messages
  (make-async-channel))

(define-system worker
  [broker (database) make-broker]
  [database (make-database-factory
             (lambda ()
               (postgresql-connect #:database "koyo"
                                   #:user     "koyo"
                                   #:password "koyo")))]
  [worker (broker) (make-worker-factory)])

(define-job (add x y)
  (+ x y))

(define-job (publish message)
  (async-channel-put messages message))

(define retried? #f)
(define-job (do-retry-once)
  (cond
    [retried?
     (async-channel-put messages 'retried)]

    [else
     (set! retried? #t)
     (retry 5000)]))

(define job-tests
  (test-suite
   "job"

   (system-test-suite job
     ([broker (database) make-broker]
      [database (make-database-factory (lambda ()
                                         (postgresql-connect #:database "koyo"
                                                             #:user     "koyo"
                                                             #:password "koyo")))])

     #:before
     (lambda _
       (with-database-connection [conn database]
         (query-exec conn "truncate table koyo_jobs")))

     (test-case "jobs can be executed synchronously"
       (parameterize ([execute-jobs-synchronously? #t])
         (check-false (add 1 2))))

     (test-case "jobs can be {en,de}queued"
       (parameterize ([current-broker broker])
         (check-not-false (add 1 2)))

       (match-define (list (vector id queue job arguments attempts))
         (broker-dequeue! broker 0 "default"))

       (check-equal?
        (list null null '(1 2))
        (deserialize arguments))

       (broker-mark-done! broker id)
       (check-equal? (broker-dequeue! broker 0 "default") null)))

   (let ([broker #f]
         [database #f])
     (test-suite
      "worker"

      #:before
      (lambda _
        (system-start worker-system)
        (set! broker (system-ref worker-system 'broker))
        (set! database (system-ref worker-system 'database))
        (with-database-connection [conn database]
          (query-exec conn "truncate table koyo_jobs")))

      #:after
      (lambda _
        (system-stop worker-system))

      (test-case "jobs can be executed"
        (parameterize ([current-broker broker])
          (publish 'hello)
          (check-equal? (sync messages) 'hello)))

      (test-case "jobs can be retried"
        (parameterize ([current-broker broker])
          (do-retry-once)
          (check-equal? (sync messages) 'retried)))))))

(module+ test
  (require rackunit/text-ui)
  (when (equal? (getenv "KOYO_DATABASE_TESTS") "x")
    (run-tests job-tests)))
