#lang racket/base

(require component/testing
         koyo/testing
         rackunit

         app-name-here/components/user
         "../common.rkt")

(provide
 user-tests)

(define the-user #f)
(define user-tests
  (system-test-suite user ([db make-test-database]
                           [users (db) make-user-manager])
    #:before
    (lambda _
      (truncate-tables! db 'users)
      (set! the-user (make-test-user! users)))

    (test-suite
     "user-manager-create!"

     (test-case "creates users"
       (check-match
        the-user
        (user (? exact-positive-integer?) "bogdan@example.com" _ #f _ _ _))))

    (test-suite
     "user-manager-login"

     (test-case "returns #f when given invalid login data"
       (check-false (user-manager-login users "invalid" "invalid"))
       (check-false (user-manager-login users "bogdan@example.com" "invalid")))

     (test-case "returns a user upon successful login"
       (check-match
        (user-manager-login users "bogdan@example.com" "hunter2")
        (user (? exact-positive-integer?) "bogdan@example.com" _ _ _ _ _))))))

(module+ test
  (run-db-tests user-tests))
