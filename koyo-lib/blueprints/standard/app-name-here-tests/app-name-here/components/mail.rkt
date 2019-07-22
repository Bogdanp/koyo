#lang racket/base

(require component
         component/testing
         koyo/url
         koyo/testing
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
                           [mailer make-test-mailer]
                           [users (db mailer) (lambda (db _)
                                                (make-user-manager db))])

   #:before
   (lambda _
     (truncate-tables! db 'users))

   (test-suite
    "mailer-send-welcome-email"

    (test-case "welcome emails contain valid verification urls"
      (define adapter (mailer-adapter mailer))
      (define the-user (make-test-user! users))
      (mailer-send-welcome-email mailer the-user)
      (check-equal?
       (~> (car (stub-mail-adapter-outbox adapter))
           (hash-ref 'template-model)
           (hash-ref 'action_url))
       (make-application-url "verify"
                             (number->string (user-id the-user))
                             (user-verification-code the-user)))))))

(module+ test
  (require rackunit/text-ui)
  (run-db-tests mail-tests))
