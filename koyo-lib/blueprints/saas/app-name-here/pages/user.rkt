#lang racket/base

(require koyo/database
         koyo/guard
         racket/contract/base
         "../components/auth.rkt"
         "../components/http.rkt"
         "../components/json.rkt"
         "../components/mail.rkt"
         "../components/user.rkt"
         "common.rkt")

(provide
 (contract-out
  [create-password-reset-token-page (-> database? mailer? user-manager? (-> request? response?))]
  [reset-password-page (-> user-manager? (-> request? id/c response?))]
  [create-user-page (-> database? mailer? user-manager? (-> request? response?))]
  [verify-user-page (-> user-manager? (-> request? id/c string? response?))]
  [me-page (-> user-manager? (-> request? response?))]))

(define ((create-password-reset-token-page db m um) req)
  (with-guard bad-request
    (define data (guard (request-json req)))
    (with-database-transaction [_ db]
      (define-values (u t)
        (create-password-reset-token!
         #:username (guard (if-null (hash-ref data 'username #f) #f))
         #:ip-address (request-ip-address req)
         #:user-agent (guard (request-headers-ref* req #"user-agent"))
         um))
      (when (and u t)
        (send-password-reset-email m u t)))
    (response/empty)))

(define ((reset-password-page um) req id)
  (with-guard bad-request
    (define data (guard (request-json req)))
    (reset-user-password!
     #:user-id id
     #:token (guard (if-null (hash-ref data 'token #f) #f))
     #:password (guard (if-null (hash-ref data 'password #f) #f))
     um)
    (response/empty)))

(define ((create-user-page db m um) req)
  (with-guard bad-request
    (define data (guard (request-json req)))
    (define username (guard (if-null (hash-ref data 'username #f) #f)))
    (guard (regexp-match? #rx".+@.+" username) #:else (bad-request "Username must be a valid e-mail address."))
    (define password (guard (if-null (hash-ref data 'password #f) #f)))
    (guard (>= (string-length password) 8) #:else (bad-request "Password must be at least 8 characters long."))
    (with-handlers ([exn:fail:user-manager:username-taken?
                     (λ (_) (bad-request "This username is taken."))])
      (with-database-transaction [_ db]
        (define u (create-user! um username password))
        (send-welcome-email m u))
      (response/empty))))

(define ((verify-user-page um) _req uid token)
  (verify-user! um uid token)
  (redirect-to "/"))

(define ((me-page um) _req)
  (response/jsexpr (->jsexpr (lookup-user-by-id um (current-user-id)))))
