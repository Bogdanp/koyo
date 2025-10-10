#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         component
         data/pool
         db
         (only-in db/private/generic/interfaces
                  connection<%>
                  statement-binding-pst)
         db/util/postgresql
         gregor
         json
         racket/class
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
        [#:log-statements? boolean?
         #:max-connections exact-positive-integer?
         #:connection-idle-ttl (or/c +inf.0 exact-positive-integer?)
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

(struct database
  (connection-pool
   connector ;; noqa
   max-connections ;; noqa
   connection-idle-ttl ;; noqa
   log-statements?)
  #:methods gen:component
  [(define (component-start db) ;; noqa
     (match-define (database _ connector max-size idle-ttl _) db)
     (define pool
       (make-pool
        #:max-size max-size
        #:idle-ttl idle-ttl
        (lambda ()
          ;; This procedure must not raise an exception. So, on connect failure, return
          ;; a dummy object whose connected? method re-raises the exception. This makes
          ;; database-borrow-connection raise at the appropriate time.
          (with-handlers ([exn:fail? (λ (e) (new dummy-connection% [p pool] [e e]))])
            (connector)))
        disconnect))
     (struct-copy database db [connection-pool pool]))

   (define (component-stop db) ;; noqa
     (pool-close! (database-connection-pool db))
     (struct-copy database db [connection-pool #f]))])

(define ((make-database-factory connector
                                #:log-statements? [log-statements? #f]
                                #:max-connections [max-connections 16]
                                #:connection-idle-ttl [connection-idle-ttl (* 60 1000)]
                                #:max-idle-connections [_max-idle-connections 2]))
  (database
   #;connection-pool #f
   #;connector connector
   #;max-connections max-connections
   #;connection-idle-ttl connection-idle-ttl
   #;log-statements log-statements?))

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
             (define the-conn (database-borrow-connection db))
             (set! conn the-conn)
             (set! close (λ () (database-release-connection db the-conn)))])))
      (lambda ()
        (with-timing "proc"
          (parameterize ([current-database-connection conn])
            (proc (if (database-log-statements? db)
                      (new logged-connection% [conn conn])
                      conn)))))
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
  (define the-pool
    (database-connection-pool db))
  (define the-conn
    (pool-take! the-pool))
  (cond
    [(send the-conn connected?)
     the-conn]
    [else
     (pool-abandon! the-pool the-conn)
     (database-borrow-connection db)]))

(define (database-release-connection db conn)
  (pool-release! (database-connection-pool db) conn))

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

(define dummy-connection%
  (class* object% (connection<%>)
    (init-field p e)
    (super-new)

    (define-syntax-rule (define-dummies [id arg ...] ...)
      (begin
        (define/public (id arg ...)
          (unless abandoned?
            (set! abandoned? #t)
            (pool-abandon! p this))
          (raise e)) ...))

    (define abandoned? #f)

    (define-dummies
      [connected?]
      [get-dbsystem]
      [query fsym stmt cursor?]
      [prepare fsym stmt close-on-exec?]
      [fetch/cursor fsym cursor fetch-size]
      [get-base]
      [list-tables fsym schema]
      [start-transaction fsym isolation option cwt?]
      [end-transaction fsym mode cwt?]
      [transaction-status fsym]
      [free-statement pst need-lock?])

    (define/public (disconnect)
      (void))))


;; Query Logging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (proxy target-id (meth-id arg-id ...) ...)
  (begin
    (define/public (meth-id arg-id ...)
      (send target-id meth-id arg-id ...)) ...))

(define logged-connection%
  (class* object% (connection<%>)
    (init-field conn)
    (super-new)

    (define/public (query fsym stmt cursor?)
      (define logger
        (current-logger))
      (when (log-level? logger 'debug)
        (define the-stmt
          (if (statement-binding? stmt)
              (send (statement-binding-pst stmt) get-stmt)
              stmt))
        (log-message
         logger 'debug 'koyo:db-statements
         (format "~a" the-stmt)
         (list (current-thread) fsym the-stmt)))
      (send conn query fsym stmt cursor?))

    (proxy
     conn
     [connected?]
     [disconnect]
     [get-dbsystem]
     [prepare fsym stmt close-on-exec?]
     [fetch/cursor fsym cursor fetch-size]
     [get-base]
     [list-tables fsym schema]
     [start-transaction fsym isolation option cwt?]
     [end-transaction fsym mode cwt?]
     [transaction-status fsym]
     [free-statement pst need-lock?])))


;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 id/c
 maybe-id/c

 if-null
 null-if

 exn:fail:sql:constraint-violation?

 (contract-out
  [in-rows (-> connection? statement? any/c ... sequence?)]
  [in-row (-> connection? statement? any/c ... sequence?)]
  [sql-date->date (-> sql-date? date?)]
  [sql-timestamp->moment (-> sql-timestamp? moment?)]
  [->sql-date (-> date-provider? sql-date?)]
  [->sql-timestamp (-> time-provider? sql-timestamp?)]
  [seconds->sql-timestamp (->* [real?] [boolean?] sql-timestamp?)]))

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

(define (seconds->sql-timestamp ts [local-time? #t])
  (match-define (date* s m h D M Y _ _ _ tz ns _)
    (seconds->date ts local-time?))
  (sql-timestamp Y M D h m s ns tz))

(define-syntax-rule (if-null v d)
  (let ([tmp v])
    (cond
      [(sql-null? tmp) d]
      [(equal? tmp (json-null)) d]
      [else tmp])))

(define (null-if v e)
  (if (equal? v e) sql-null v))
