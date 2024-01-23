#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         component
         db
         db/util/postgresql
         gregor
         racket/contract/base
         racket/match
         racket/sequence
         "profiler.rkt")

(provide
 (contract-out
  [current-database-connection (parameter/c (or/c #f connection?))]
  [database? (-> any/c boolean?)]
  [make-database-factory
   (->* [(-> connection?)]
        [#:max-connections exact-positive-integer?
         #:max-idle-connections exact-positive-integer?]
        (-> database?))]
  [call-with-database-connection
    (-> database? (-> connection? any) any)]
  [call-with-database-transaction
    (->* [database? (-> connection? any)]
         [#:isolation (or/c #f
                            'serializable
                            'repeatable-read
                            'read-committed
                            'read-uncommitted)]
         any)]
  [database-borrow-connection (-> database? connection?)]
  [database-release-connection (-> database? connection? void?)])

 with-database-connection
 with-database-transaction)

(struct database (connection-pool options)
  #:methods gen:component
  [(define (component-start db)
     (define options (database-options db))
     (define connect (hash-ref options 'connector))
     (define pool
       (connection-pool
        #:max-connections (hash-ref options 'max-connections)
        #:max-idle-connections (hash-ref options 'max-idle-connections)
        connect))
     (struct-copy database db [connection-pool pool]))

   (define (component-stop db)
     (struct-copy database db [connection-pool #f]))])

(define ((make-database-factory connector
                                #:max-connections [max-connections 16]
                                #:max-idle-connections [max-idle-connections 2]))
  (database #f (hasheq 'connector connector
                       'max-connections max-connections
                       'max-idle-connections max-idle-connections)))

(define current-database-connection
  (make-parameter #f))

(define (call-with-database-connection db proc)
  (with-timing 'database "call-with-database-connection"
    (define conn #f)
    (define close void)
    (dynamic-wind
      (lambda ()
        (with-timing "lease"
          (cond
            [(current-database-connection)
             => (lambda (the-conn)
                  (set! conn the-conn)
                  (set! close void))]
            [else
             (define the-conn
               (connection-pool-lease
                (database-connection-pool db)))
             (set! conn the-conn)
             (set! close (λ () (disconnect the-conn)))])))
      (lambda ()
        (with-timing "proc"
          (parameterize ([current-database-connection conn])
            (proc conn))))
      (lambda ()
        (with-timing "disconnect"
          (close))))))

(define (call-with-database-transaction db proc #:isolation [isolation #f])
  (with-timing 'database "call-with-database-transaction"
    (with-database-connection [conn db]
      (call-with-transaction conn
        #:isolation isolation
        (lambda ()
          (proc conn))))))

(define (database-borrow-connection db)
  (connection-pool-lease (database-connection-pool db)))

(define (database-release-connection _db conn)
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

 (contract-out
  [in-rows (-> connection? statement? any/c ... sequence?)]
  [in-row (-> connection? statement? any/c ... sequence?)]
  [sql-date->date (-> sql-date? date?)]
  [sql-timestamp->moment (-> sql-timestamp? moment?)]
  [->sql-date (-> date-provider? sql-date?)]
  [->sql-timestamp (-> time-provider? sql-timestamp?)]))

(define id/c exact-positive-integer?)
(define maybe-id/c (or/c #f id/c))

(define exn:fail:sql:constraint-violation?
  (match-lambda
    [(exn:fail:sql _ _ (or "23503" "23505") _) #t]
    [_ #f]))

(define (in-rows conn stmt . args)
  (sequence-map
   (lambda cols
     (apply values (map sql-> cols)))
   (apply in-query conn stmt args)))

(define (in-row conn stmt . args)
  (define consumed? #f)
  (stop-before
   (apply in-rows conn stmt args)
   (lambda _args
     (begin0 consumed?
       (set! consumed? #t)))))

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
    [else
     v]))

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

(define (->sql-date m)
  (sql-date (->year m)
            (->month m)
            (->day m)))

(define (->sql-timestamp m)
  (sql-timestamp (->year m)
                 (->month m)
                 (->day m)
                 (->hours m)
                 (->minutes m)
                 (->seconds m)
                 (->nanoseconds m)
                 (->utc-offset m)))
