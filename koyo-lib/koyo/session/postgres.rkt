#lang racket/base

(require buid
         db
         db/define-query
         racket/contract/base
         racket/fasl
         racket/match
         racket/serialize
         "../database.rkt"
         "../session.rkt")

(provide
 (contract-out
  [postgres-session-store? (-> any/c boolean?)]
  [make-postgres-session-store
   (->* [database?]
        [#:ttl exact-positive-integer?]
        session-store?)]))

(define (make-postgres-session-store database #:ttl [ttl (* 7 86400)])
  (postgres-session-store database (~ttl ttl)))

(struct postgres-session-store (database ttl)
  #:methods gen:session-store
  [(define (session-store-generate-id! _ss)
     (buid))

   (define (session-store-load! ss)
     (with-database-transaction [conn (postgres-session-store-database ss)]
       (query-exec conn create-sessions-table)
       (query-exec conn create-session-data-table)))

   (define (session-store-persist! _ss)
     (void))

   (define (session-store-ref ss session-id key default)
     (match-define (postgres-session-store db ttl) ss)
     (with-database-transaction [conn db]
       (query-exec conn expire)
       (define uuid (buid->uuid session-id))
       (define key-str (symbol->string key))
       (match (query-maybe-row conn lookup uuid key-str)
         [#f (if (procedure? default) (default) default)]
         [(vector data) (begin0 (deserialize* data)
                          (query-exec conn touch uuid ttl))])))

   (define (session-store-set! ss session-id key value)
     (match-define (postgres-session-store db ttl) ss)
     (with-database-transaction [conn db]
       (define uuid (buid->uuid session-id))
       (define key-str (symbol->string key))
       (query-exec conn touch uuid ttl)
       (query-exec conn update uuid key-str (serialize* value))))

   (define (session-store-update! ss session-id key updater default)
     (match-define (postgres-session-store db _) ss)
     (with-database-transaction [_ db]
       (define value (session-store-ref ss session-id key default))
       (session-store-set! ss session-id key (updater value))))

   (define (session-store-remove! ss session-id key)
     (match-define (postgres-session-store db _) ss)
     (with-database-transaction [conn db]
       (define uuid (buid->uuid session-id))
       (define key-str (symbol->string key))
       (query-exec conn delete uuid key-str)))])

(define-query-definer define-query
  "postgres-queries")

(define-query
  create-sessions-table
  create-session-data-table
  delete expire lookup touch update)

(define serialize*
  (compose1 s-exp->fasl serialize))

(define deserialize*
  (compose1 deserialize fasl->s-exp))

(define (~ttl ttl)
  (sql-interval
   #;years 0
   #;months 0
   #;days 0
   #;hours 0
   #;minutes 0
   #;seconds ttl
   #;nanoseconds 0))
