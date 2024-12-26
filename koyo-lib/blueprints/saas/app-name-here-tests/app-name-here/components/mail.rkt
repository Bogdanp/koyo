#lang racket/base

(require component
         component/testing
         koyo/testing
         koyo/url
         rackunit
         threading

         app-name-here/components/mail
         app-name-here/components/user
         "../common.rkt")

(provide
 mail-tests)

(define mailer
  (make-test-mailer))

(define mail-tests
  (system-test-suite mail ([db make-test-database]
                           [hasher make-test-hasher]
                           [mailer make-test-mailer]
                           [users (db hasher mailer) (lambda (db h _)
                                                       (make-user-manager db h))])

    #:before
    (lambda ()
      (truncate-tables! db 'users))

    (test-suite
     "send-welcome-email"

     (test-case "welcome emails contain valid verification urls"
       (define adapter (mailer-adapter mailer))
       (define the-user (make-test-user! users))
       (send-welcome-email mailer the-user)
       (check-equal?
        (~> (car (stub-mail-adapter-outbox adapter))
            (hash-ref 'template-model)
            (hash-ref 'action_url))
        (make-application-url "verify"
                              (number->string (user-id the-user))
                              (user-verification-code the-user)))))))

(module+ test
  (run-db-tests mail-tests))
