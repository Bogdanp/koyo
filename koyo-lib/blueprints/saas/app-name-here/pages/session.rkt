#lang racket/base

(require koyo/database
         koyo/guard
         racket/contract/base
         "../components/auth.rkt"
         "../components/http.rkt"
         "common.rkt")

(provide
 (contract-out
  [login-page (-> auth-manager? (-> request? response?))]
  [logout-page (-> auth-manager? (-> request? response?))]))

(define ((login-page am) req)
  (with-guard (λ () (bad-request))
    (define data (guard (request-json req)))
    (define username (guard (if-null (hash-ref data 'username #f) #f)))
    (define password (guard (if-null (hash-ref data 'password #f) #f)))
    (with-handlers ([exn:fail:auth-manager:unverified?
                     (λ (_) (bad-request "You haven't verified your account yet."))])
      (guard
       (login! am username password)
       #:else (bad-request "Invalid username or password."))
      (response/empty))))

(define ((logout-page am) _req)
  (logout! am)
  (redirect-to "/"))
