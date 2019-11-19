#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         component
         db
         db/util/postgresql
         gregor
         racket/contract
         racket/match
         racket/sequence
         "profiler.rkt")

(provide
 current-database-connection
 make-database-factory
 database?
 call-with-database-connection
 call-with-database-transaction
 database-borrow-connection
 database-release-connection

 with-database-connection
 with-database-transaction)

(struct database (connection-pool options)
  #:methods gen:component
  [(define (component-start a-database)
     (define options (database-options a-database))
     (define connect (hash-ref options 'connector))
     (struct-copy database a-database
                  [connection-pool (connection-pool
                                    #:max-connections (hash-ref options 'max-connections)
                                    #:max-idle-connections (hash-ref options 'max-idle-connections)
                                    connect)]))

   (define (component-stop a-database)
     (struct-copy database a-database [connection-pool #f]))])

(define/contract ((make-database-factory connector
                                         #:max-connections [max-connections 16]
                                         #:max-idle-connections [max-idle-connections 2]))
  (->* ((-> connection?))
       (#:max-connections exact-positive-integer?
        #:max-idle-connections exact-positive-integer?)
       (-> database?))
  (database #f (hasheq 'connector connector
                       'max-connections max-connections
                       'max-idle-connections max-idle-connections)))

(define current-database-connection
  (make-parameter #f))

(define/contract (call-with-database-connection database proc)
  (-> database? (-> connection? any) any)
  (with-timing 'database "call-with-database-connection"
    (define-values (connection disconnecter)
      (cond
        [(current-database-connection)
         => (lambda (conn)
              (values conn void))]

        [else
         (define connection
           (connection-pool-lease (database-connection-pool database)))

         (values connection (lambda ()
                              (disconnect connection)))]))

    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (with-timing "proc"
          (parameterize ([current-database-connection connection])
            (proc connection))))
      (lambda ()
        (with-timing "disconnect"
          (disconnecter))))))

(define/contract (call-with-database-transaction database proc #:isolation [isolation #f])
  (->* (database? (-> connection? any))
       (#:isolation (or/c false/c
                          'serializable
                          'repeatable-read
                          'read-committed
                          'read-uncommitted))
       any)
  (with-timing 'database "call-with-database-transaction"
    (with-database-connection [conn database]
      (call-with-transaction conn
        #:isolation isolation
        (lambda ()
          (proc conn))))))

(define/contract (database-borrow-connection database)
  (-> database? connection?)
  (connection-pool-lease (database-connection-pool database)))

(define/contract (database-release-connection database conn)
  (-> database? connection? void?)
  (disconnect conn))

(define-syntax (with-database-connection stx)
  (syntax-parse stx
    [(_ [name:id database:expr] e:expr ...+)
     #'(call-with-database-connection database
         (lambda (name)
           e ...))]))

(define-syntax (with-database-transaction stx)
  (syntax-parse stx
    [(_ [name:id database:expr] e:expr ...+)
     #'(with-database-transaction [name database]
         #:isolation #f
         e ...)]

    [(_ [name:id database:expr] #:isolation isolation e:expr ...+)
     #'(call-with-database-transaction database
         #:isolation isolation
         (lambda (name)
           e ...))]))


;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 id/c
 maybe-id/c

 exn:fail:sql:constraint-violation?

 in-rows
 in-row

 ->sql-date
 ->sql-timestamp)

(define id/c exact-positive-integer?)
(define maybe-id/c (or/c false/c id/c))

(define/contract exn:fail:sql:constraint-violation?
  (-> any/c boolean?)
  (match-lambda
    [(exn:fail:sql _ _ (or "23503" "23505") _) #t]
    [_ #f]))

(define/contract (in-rows conn stmt . args)
  (-> connection? statement? any/c ... sequence?)
  (sequence-map (lambda cols
                  (apply values (map sql-> cols)))
                (apply in-query conn stmt args)))

(define/contract (in-row conn stmt . args)
  (-> connection? statement? any/c ... sequence?)
  (let ([consumed #f])
    (stop-before (apply in-rows conn stmt args) (lambda _
                                                  (begin0 consumed
                                                    (set! consumed #t))))))

(define (sql-> v)
  (cond
    [(sql-null? v)
     #f]

    [(pg-array? v)
     (pg-array->list v)]

    [(sql-date? v)
     (sql-date->date v)]

    [(sql-timestamp? v)
     (sql-timestamp->moment v)]

    [else v]))

(define (sql-date->date d)
  (date (sql-date-year d)
        (sql-date-month d)
        (sql-date-day d)))

(define (sql-timestamp->moment t)
  (moment (sql-timestamp-year t)
          (sql-timestamp-month t)
          (sql-timestamp-day t)
          (sql-timestamp-hour t)
          (sql-timestamp-minute t)
          (sql-timestamp-second t)
          (sql-timestamp-nanosecond t)
          #:tz (or (sql-timestamp-tz t) (current-timezone))))

(define/contract (->sql-date m)
  (-> date-provider? sql-date?)
  (sql-date (->year m)
            (->month m)
            (->day m)))

(define/contract (->sql-timestamp m)
  (-> time-provider? sql-timestamp?)
  (sql-timestamp (->year m)
                 (->month m)
                 (->day m)
                 (->hours m)
                 (->minutes m)
                 (->seconds m)
                 (->nanoseconds m)
                 (->utc-offset m)))
