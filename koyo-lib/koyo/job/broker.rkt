#lang racket/base

(require component
         db
         racket/contract
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

(define (broker-borrow-connection broker)
  (database-borrow-connection (broker-database broker)))

(define (broker-release-connection broker conn)
  (database-release-connection (broker-database broker) conn))

(define (broker-enqueue! broker queue job priority arguments)
  (with-database-connection [conn (broker-database broker)]
    (for/first ([(id _) (in-row conn
                                enqueue-stmt
                                queue
                                (symbol->string job)
                                (serialize arguments)
                                priority)])
      id)))

(define (broker-dequeue! broker worker-id queue [n 1])
  (with-database-transaction [conn (broker-database broker)]
    (begin0 (query-rows conn dequeue-stmt worker-id queue n)
      (query-exec conn heartbeat-stmt worker-id))))

(define (broker-mark-done! broker id)
  (with-database-connection [conn (broker-database broker)]
    (query-exec conn mark-done-stmt id)))

(define (broker-mark-failed! broker id)
  (with-database-connection [conn (broker-database broker)]
    (query-exec conn mark-failed-stmt id)))

(define (broker-mark-for-retry! broker id moment)
  (with-database-connection [conn (broker-database broker)]
    (query-exec conn mark-for-retry-stmt id (->sql-timestamp moment))))

(define (broker-perform-maintenance! broker id)
  (with-database-transaction [conn (broker-database broker)]
    (query-exec conn heartbeat-stmt id)
    (query-exec conn unregister-stale-workers-stmt)))

(define (broker-register-worker! broker pid hostname)
  (with-database-transaction [conn (broker-database broker)]
    (define id (query-value conn register-worker-stmt pid hostname))
    (begin0 id
      (broker-perform-maintenance! broker id))))

(define (broker-unregister-worker! broker id)
  (with-database-connection [conn (broker-database broker)]
    (query-exec conn unregister-worker-stmt id)))


;; for the admin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 broker-jobs)

(define (broker-jobs broker [cursor -1])
  (with-database-connection [conn (broker-database broker)]
    (for/list ([(id queue job arguments status) (in-query conn latest-jobs-stmt cursor)])
      (hasheq 'id id
              'queue queue
              'job job
              'arguments arguments
              'status status))))
