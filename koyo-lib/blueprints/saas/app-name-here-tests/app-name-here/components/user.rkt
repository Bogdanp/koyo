#lang racket/base

(require component/testing
         koyo/testing
         racket/match
         rackunit

         app-name-here/components/user
         "../common.rkt")

(provide
 user-tests)

(define the-user #f)
(define user-tests
  (system-test-suite user ([db make-test-database]
                           [hasher make-test-hasher]
                           [users (db hasher) make-user-manager]
                           [migrator (db) make-test-migrator])
    #:before
    (lambda ()
      (truncate-tables! db 'users)
      (set! the-user (make-test-user! users)))

    (test-suite
     "create-user!"

     (test-case "creates users"
       (check-match
        the-user
        (struct*
         user
         ([id (? exact-positive-integer?)]
          [username "bogdan@example.com"]
          [verified? #f])))))

    (test-suite
     "create-password-reset-token!"

     (test-case "returns #f given an invalid username"
       (define-values (_ token)
         (create-password-reset-token!
          #:username "idontexist"
          #:ip-address "127.0.0.1"
          #:user-agent "Mozilla"
          users))
       (check-false token))

     (test-case "returns a token given a valid user"
       (define-values (_ token)
         (create-password-reset-token!
          #:username (user-username the-user)
          #:ip-address "127.0.0.1"
          #:user-agent "Mozilla"
          users))

       (check-not-false token)

       (test-case "invalidates old tokens"
         (define-values (_ token*)
           (create-password-reset-token!
            #:username (user-username the-user)
            #:ip-address "127.0.0.1"
            #:user-agent "Mozilla"
            users))

         (check-false
          (reset-user-password!
           #:user-id (user-id the-user)
           #:token token
           #:password "new-password"
           users))

         (check-true
          (reset-user-password!
           #:user-id (user-id the-user)
           #:token token*
           #:password "hunter2"
           users))

         (test-case "tokens cannot be reused"
           (check-false
            (reset-user-password!
             #:user-id (user-id the-user)
             #:token token*
             #:password "hunter2"
             users))))))

    (test-suite
     "login"

     (test-case "returns #f when given invalid login data"
       (check-false (login users "invalid" "invalid"))
       (check-false (login users "bogdan@example.com" "invalid")))

     (test-case "returns a user upon successful login"
       (check-match
        (login users "bogdan@example.com" "hunter2")
        (struct* user ([id (? exact-positive-integer?)]
                       [username "bogdan@example.com"])))))))

(module+ test
  (run-db-tests user-tests))
