#lang racket/base

(require koyo/mail
         koyo/url
         racket/contract/base
         racket/string
         "user.rkt")

(provide
 (all-from-out koyo/mail)
 (contract-out
  [mailer-send-welcome-email (-> mailer? user? void?)]
  [mailer-send-password-reset-email (-> mailer? user? non-empty-string? void?)]))

(define (mailer-send-welcome-email m user)
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

(define (mailer-send-password-reset-email m user token)
  (define action-url
    (make-application-url "password-reset" (number->string (user-id user)) token))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to (user-username user)
   #:from (mailer-sender m)
   #:template-alias "password-reset"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'name (user-username user)
                      'username (user-username user))))

;; Local Variables:
;; eval: (put 'mailer-merge-common-variables 'racket-indent-function #'begin)
;; End:
