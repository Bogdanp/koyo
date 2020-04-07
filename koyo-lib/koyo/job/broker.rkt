#lang racket/base

(require component
         db
         racket/contract
         racket/sequence
         "../database.rkt"
         "query.rkt"
         "schema.rkt"
         "serialize.rkt")

(provide
 current-broker
 make-broker
 broker?
 broker-borrow-connection
 broker-release-connection
 broker-enqueue!
 broker-dequeue!
 broker-mark-done!
 broker-mark-failed!
 broker-mark-for-retry!
 broker-perform-maintenance!
 broker-register-worker!
 broker-unregister-worker!)

(struct broker (database)
  #:transparent
  #:methods gen:component
  [(define (component-start b)
     (begin0 b
       (ensure-latest-schema! (broker-database b))))

   (define component-stop values)])

(define/contract (make-broker database)
  (-> database? broker?)
  (broker database))

(define/contract current-broker
  (parameter/c (or/c false/c broker?))
  (make-parameter #f))

(define (broker-borrow-connection b)
  (database-borrow-connection (broker-database b)))

(define (broker-release-connection b conn)
  (database-release-connection (broker-database b) conn))

(define (broker-enqueue! b queue job priority scheduled-at arguments)
  (with-database-connection [conn (broker-database b)]
    (for/first ([(id _) (in-row conn
                                enqueue-stmt
                                queue
                                (symbol->string job)
                                (serialize arguments)
                                priority
                                (->sql-timestamp scheduled-at))])
      id)))

(define (broker-dequeue! b worker-id queue [n 1])
  (with-database-transaction [conn (broker-database b)]
    (begin0 (query-rows conn dequeue-stmt worker-id queue n)
      (query-exec conn heartbeat-stmt worker-id))))

(define (broker-mark-done! b id)
  (with-database-connection [conn (broker-database b)]
    (query-exec conn mark-done-stmt id)))

(define (broker-mark-failed! b id)
  (with-database-connection [conn (broker-database b)]
    (query-exec conn mark-failed-stmt id)))

(define (broker-mark-for-retry! b id moment)
  (with-database-connection [conn (broker-database b)]
    (query-exec conn mark-for-retry-stmt id (->sql-timestamp moment))))

(define (broker-perform-maintenance! b id)
  (with-database-transaction [conn (broker-database b)]
    (query-exec conn heartbeat-stmt id)
    (query-exec conn unregister-stale-workers-stmt)))

(define (broker-register-worker! b pid hostname)
  (with-database-transaction [conn (broker-database b)]
    (define id (query-value conn register-worker-stmt pid hostname))
    (begin0 id
      (broker-perform-maintenance! b id))))

(define (broker-unregister-worker! b id)
  (with-database-connection [conn (broker-database b)]
    (query-exec conn unregister-worker-stmt id)))


;; for the admin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out job-meta)
 broker-job
 broker-jobs)

(struct job-meta (id queue job arguments status priority attempts created-at scheduled-at started-at worker-id)
  #:transparent)

(define (broker-job b id)
  (with-database-connection [conn (broker-database b)]
    (for/first ([job (in-jobs conn lookup-job-stmt id)])
      job)))

(define (broker-jobs b [cursor -1])
  (with-database-connection [conn (broker-database b)]
    (for/list ([job (in-jobs conn latest-jobs-stmt cursor)])
      job)))

(define (make-job-meta id queue job arguments status priority attempts created-at scheduled-at started-at worker-id)
  (job-meta id queue job (deserialize arguments) status priority attempts created-at scheduled-at started-at worker-id))

(define (in-jobs conn q . args)
  (sequence-map make-job-meta (apply in-rows conn q args)))
