#lang racket/base

(require component
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
  (test-suite
   "mail"
   #:before
   (lambda _
     (set! mailer (component-start mailer)))

   #:after
   (lambda _
     (set! mailer (component-stop mailer)))

   (test-suite
    "mailer-send-welcome-email"

    (test-case "welcome emails contain valid verification urls"
      (define adapter (mailer-adapter mailer))
      (define a-user (make-user #:id 1 #:username "someone@example.com"))
      (mailer-send-welcome-email mailer a-user)
      (check-equal?
       (~> (car (stub-mail-adapter-outbox adapter))
           (hash-ref 'template-model)
           (hash-ref 'action_url))
       (make-application-url "verify" "1" (user-verification-code a-user)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests mail-tests))
