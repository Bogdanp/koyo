#lang racket/base

(require koyo/mail
         koyo/url
         racket/contract/base
         racket/string
         "user.rkt"
         "workspace-invite.rkt"
         "workspace.rkt")

(provide
 (all-from-out koyo/mail)
 (contract-out
  [send-welcome-email (-> mailer? user? void?)]
  [send-password-reset-email (-> mailer? user? non-empty-string? void?)]
  [send-workspace-invite-email (-> mailer? workspace? workspace-invite? void?)]))

(define (send-welcome-email m user)
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

(define (send-password-reset-email m user token)
  (define action-url
    (make-application-url "reset-password" (number->string (user-id user)) token))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to (user-username user)
   #:from (mailer-sender m)
   #:template-alias "password-reset"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'name (user-username user)
                      'username (user-username user))))

(define (send-workspace-invite-email m w wi)
  (define action-url
    (make-application-url
     "workspaces"
     (number->string (workspace-id w))
     "join"
     (workspace-invite-token wi)))
  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to (workspace-invite-email wi)
   #:from (mailer-sender m)
   #:template-alias "workspace-invite"
   #:template-model (mailer-merge-common-variables m
                      'action_url action-url
                      'workspace (workspace-name w))))

;; Local Variables:
;; eval: (put 'mailer-merge-common-variables 'racket-indent-function #'begin)
;; End:
