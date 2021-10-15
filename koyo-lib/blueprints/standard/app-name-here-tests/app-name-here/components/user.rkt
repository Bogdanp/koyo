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
     "user-manager-create!"

     (test-case "creates users"
       (check-match
        the-user
        (struct* user ([id (? exact-positive-integer?)]
                       [username "bogdan@example.com"]
                       [verified? #f])))))

    (test-suite
     "user-manager-create-reset-token!"

     (test-case "returns #f given an invalid username"
       (define-values (_ token)
         (user-manager-create-reset-token! users
                                           #:username "idontexist"
                                           #:ip-address "127.0.0.1"
                                           #:user-agent "Mozilla"))
       (check-false token))

     (test-case "returns a token given a valid user"
       (define-values (_ token)
         (user-manager-create-reset-token! users
                                           #:username (user-username the-user)
                                           #:ip-address "127.0.0.1"
                                           #:user-agent "Mozilla"))

       (check-not-false token)

       (test-case "invalidates old tokens"
         (define-values (_ token*)
           (user-manager-create-reset-token! users
                                             #:username (user-username the-user)
                                             #:ip-address "127.0.0.1"
                                             #:user-agent "Mozilla"))

         (check-false
          (user-manager-reset-password! users
                                        #:user-id (user-id the-user)
                                        #:token token
                                        #:password "new-password"))

         (check-true
          (user-manager-reset-password! users
                                        #:user-id (user-id the-user)
                                        #:token token*
                                        #:password "hunter2"))

         (test-case "tokens cannot be reused"
           (check-false
            (user-manager-reset-password! users
                                          #:user-id (user-id the-user)
                                          #:token token*
                                          #:password "hunter2"))))))

    (test-suite
     "user-manager-login"

     (test-case "returns #f when given invalid login data"
       (check-false (user-manager-login users "invalid" "invalid"))
       (check-false (user-manager-login users "bogdan@example.com" "invalid")))

     (test-case "returns a user upon successful login"
       (check-match
        (user-manager-login users "bogdan@example.com" "hunter2")
        (struct* user ([id (? exact-positive-integer?)]
                       [username "bogdan@example.com"])))))))

(module+ test
  (run-db-tests user-tests))
