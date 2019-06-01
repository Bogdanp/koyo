#lang racket/base

(require koyo/mail
         koyo/url
         racket/contract
         "user.rkt")

(provide
 (all-from-out koyo/mail)
 mailer-send-welcome-email)

(define/contract (mailer-send-welcome-email m user)
  (-> mailer? user? void?)

  (define action-url
    (make-application-url "verify" (number->string (user-id user)) (user-verification-code user)))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to (user-username user)
   #:from (mailer-sender m)
   #:template-alias "welcome"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'name (user-username user)
                      'username (user-username user))))

(module+ test
  (require rackunit
           rackunit/text-ui
           threading)

  (define adapter (make-stub-mail-adapter))
  (define mailer ((make-mailer #:adapter adapter
                               #:sender "support@example.com"
                               #:common-variables (hasheq))))

  (run-tests
   (test-suite
    "mail"

    (test-suite
     "mailer-send-welcome-email"

     (test-case "welcome emails contain valid verification urls"
       (define a-user (user++ #:id 1 #:username "someone@example.com"))
       (mailer-send-welcome-email mailer a-user)
       (check-equal?
        (~> (car (stub-mail-adapter-outbox adapter))
            (hash-ref 'template-model)
            (hash-ref 'action_url))
        (make-application-url "verify" "1" (user-verification-code a-user))))))))
