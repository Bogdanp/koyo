#lang racket/base

(require db
         koyo)

(provide
 make-test-database)

(define (make-test-database)
  ((make-database-factory
    (lambda ()
      (postgresql-connect
       #:server   (or (getenv "KOYO_TEST_DB_HOST") "127.0.0.1")
       #:port     (string->number (or (getenv "KOYO_TEST_DB_PORT") "5432"))
       #:database (or (getenv "KOYO_TEST_DB_NAME") "koyo")
       #:user     (or (getenv "KOYO_TEST_DB_USERNAME") "koyo")
       #:password (or (getenv "KOYO_TEST_DB_PASSWORD") "koyo"))))))
