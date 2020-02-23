#lang racket/base

(require db
         koyo/database-url
         rackunit)

(provide database-url-tests)

(define-syntax-rule (check-values (v ...) e)
  (call-with-values
   (lambda ()
     e)
   (lambda args
     (check-equal? args (list v ...)))))

(define database-url-tests
  (test-suite
   "database-url"

   (test-suite
    "parse-database-url"

    (check-exn
     (lambda (e)
       (and (exn:fail? e)
            (check-regexp-match "unsupported database" (exn-message e))))
     (lambda ()
       (parse-database-url "invalid:///")))

    (check-exn
     (lambda (e)
       (and (exn:fail? e)
            (check-regexp-match "database not provided" (exn-message e))))
     (lambda ()
       (parse-database-url "postgresql:///")))

    (check-values (sqlite3-connect #f #f 'memory #f #f)
      (parse-database-url "sqlite3:///:memory:"))

    (check-values (sqlite3-connect #f #f "db.sqlite3" #f #f)
      (parse-database-url "sqlite3:///db.sqlite3"))

    (check-values (sqlite3-connect #f #f "/path/to/db.sqlite3" #f #f)
      (parse-database-url "sqlite3:////path/to/db.sqlite3"))

    (check-values (postgresql-connect "127.0.0.1" 5432 "example" #f #f)
      (parse-database-url "postgres:///example"))

    (check-values (postgresql-connect "127.0.0.1" 15432 "example" #f #f)
      (parse-database-url "postgres://127.0.0.1:15432/example"))

    (check-values (postgresql-connect "127.0.0.1" 15432 "example" #f #f)
      (parse-database-url "postgresql://127.0.0.1:15432/example"))

    (check-values (postgresql-connect "127.0.0.1" 15432 "example" "bogdan" #f)
      (parse-database-url "postgresql://bogdan@127.0.0.1:15432/example"))

    (check-values (postgresql-connect "127.0.0.1" 15432 "example" "bogdan" "example")
      (parse-database-url "postgresql://bogdan:example@127.0.0.1:15432/example")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests database-url-tests))

;; Local Variables:
;; eval: (put 'check-values 'racket-indent-function #'defun)
;; End:
