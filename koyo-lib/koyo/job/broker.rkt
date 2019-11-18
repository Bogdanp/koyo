#lang racket/base

(require component
         db
         racket/class
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
  (with-database-connection [conn (broker-database broker)]
    (query-rows conn dequeue-stmt worker-id queue n)))

(define (broker-mark-done! broker id)
  (with-database-connection [conn (broker-database broker)]
    (query-exec conn mark-done-stmt id)))

(define (broker-mark-failed! broker id)
  (with-database-connection [conn (broker-database broker)]
    (query-exec conn mark-failed-stmt id)))

(define (broker-mark-for-retry! broker id moment)
  (with-database-connection [conn (broker-database broker)]
    (query-exec conn mark-for-retry-stmt id (->sql-timestamp moment))))

;; TODO: maintenance on register
(define (broker-register-worker! broker pid hostname)
  (with-database-connection [conn (broker-database broker)]
    (query-value conn register-worker-stmt pid hostname)))

(define (broker-unregister-worker! broker id)
  (with-database-transaction [conn (broker-database broker)]
    (query-exec conn release-worker-jobs-stmt id)
    (query-exec conn unregister-worker-stmt id)))
