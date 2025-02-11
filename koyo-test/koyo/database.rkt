#lang racket/base

(require component
         db
         gregor
         koyo/database
         koyo/database/batch
         racket/sequence
         rackunit
         "common.rkt")

(provide database-tests)

(define db
  (component-start
   (make-test-database)))

(define database-tests
  (test-suite
   "database"

   (test-suite
    "with-database-connection"

    (check-eq?
     (with-database-connection [conn db]
       (query-value conn "select 1"))
     1)

    (check-eq?
     (with-database-transaction [conn db]
       (query-value conn "select 1"))
     1)

    (check-eq?
     (with-database-transaction [conn db]
       #:isolation 'repeatable-read
       (query-value conn "select 1"))
     1))

   (test-suite
    "in-rows"

    (test-case "normalizes columns using sql-> before returning them"
      (check-equal?
       (with-database-connection [conn db]
         (sequence->list (sequence-map list (in-rows conn "select current_date, null, ARRAY[1,2,3]"))))
       (list (list (today) #f '(1 2 3))))))

   (test-suite
    "in-row"

    (test-case "returns an empty stream if there are no rows in the result"
      (check-false
       (with-database-connection [conn db]
         (for/first ([(x y) (in-row conn "select 1, 2 where false")])
           (+ x y)))))

    (test-case "returns only the first row in a result set"
      (check-equal?
       (with-database-connection [conn db]
         (for/list ([(x y) (in-row conn "select 1, 2 union select 3, 4")])
           (cons x y)))
       '((1 . 2)))))

   (test-suite
    "insert-batcher"
    #:before (lambda ()
               (with-database-connection [conn db]
                 (query-exec conn "CREATE TEMPORARY TABLE ib_test(a INTEGER PRIMARY KEY, b TEXT, c REAL)")))
    #:after (lambda ()
              (with-database-connection [conn db]
                (query-exec conn "DROP TABLE ib_test")))

    (test-case "does nothing on empty flush"
      (define ib
        (make-insert-batcher
         'ib_test
         '([a "INTEGER"]
           [b "TEXT"]
           [c "REAL"])))
      (with-database-connection [conn db]
        (check-equal?
         (ib-flush! ib conn)
         (void))))

    (test-case "inserts data"
      (define ib
        (make-insert-batcher
         #:batch-size 100
         'ib_test
         '([a "INTEGER"]
           [b "TEXT"]
           [c "REAL"])))
      (with-database-connection [conn db]
        (for ([idx (in-range 201)])
          (ib-push! ib conn idx (format "row ~a" idx) (* 1.0 idx)))
        (ib-flush! ib conn)
        (check-equal?
         (query-value conn "SELECT COUNT(*) FROM ib_test")
         201)))

    (test-case "handles conflicts"
      (define ib
        (make-insert-batcher
         #:batch-size 100
         #:on-conflict '(do-nothing (a))
         'ib_test
         '([a "INTEGER"]
           [b "TEXT"]
           [c "REAL"])))
      (with-database-connection [conn db]
        (for ([idx (in-range 300)])
          (ib-push! ib conn idx (format "row ~a" idx) (* 1.0 idx)))
        (ib-flush! ib conn)
        (check-equal?
         (query-value conn "SELECT COUNT(*) FROM ib_test")
         300)))

    (test-case "complains when pushing fewer cols than expected"
      (define ib
        (make-insert-batcher
         'ib_test
         '([a "INTEGER"]
           [b "TEXT"])))
      (check-exn
       #rx"expected: at least 3 non-keyword arguments"
       (lambda ()
         (with-database-connection [conn db]
           (ib-push! ib conn))))))))

(module+ test
  (require rackunit/text-ui)
  (when (equal? (getenv "KOYO_DATABASE_TESTS") "x")
    (run-tests database-tests)))
