#lang racket/base

(require component
         db
         gregor
         koyo/database
         racket/sequence
         rackunit)

(provide database-tests)

(define db
  (component-start
   ((make-database-factory #:database "koyo"
                           #:username "koyo"
                           #:password "koyo"))))

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
       '((1 . 2)))))))

(module+ test
  (require rackunit/text-ui)
  (when (equal? (getenv "KOYO_DATABASE_TESTS") "x")
    (run-tests database-tests)))
