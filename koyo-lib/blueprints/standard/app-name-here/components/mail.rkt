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
