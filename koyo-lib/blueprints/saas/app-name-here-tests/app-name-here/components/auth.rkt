#lang racket/base

(require component/testing
         koyo/session
         koyo/testing
         rackunit

         app-name-here/components/auth
         app-name-here/components/user
         "../common.rkt")

(provide
 auth-tests)

(define user #f)
(define auth-tests
  (system-test-suite auth ([auth (sessions users) make-auth-manager]
                           [db make-test-database]
                           [hasher () make-test-hasher]
                           [migrator (db) make-test-migrator]
                           [sessions make-test-session-manager]
                           [users (db hasher) make-user-manager])
    #:before
    (lambda ()
      (truncate-tables! db 'users)
      (set! user (create-user! users "bogdan" "hunter2")))

    (test-suite
     "auth-manager-login"

     (test-case "returns #f if the user does not exist"
       (check-false
        (login! auth "idontexist" "hunter2")))

     (test-case "returns #f if the password is wrong"
       (check-false
        (login! auth "bogdan" "invalid")))

     (test-case "fails if the user is not verified"
       (check-exn
        exn:fail:auth-manager:unverified?
        (lambda ()
          (login! auth "bogdan" "hunter2"))))

     (test-case "returns the user when verified"
       (parameterize ([current-session-manager sessions]
                      [current-session-id "fake"])
         (verify-user! users (user-id user) (user-verification-code user))
         (check-equal? (user-id user)
                       (user-id (login! auth "bogdan" "hunter2"))))))))

(module+ test
  (run-db-tests auth-tests))
