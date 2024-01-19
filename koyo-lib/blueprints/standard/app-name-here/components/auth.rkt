#lang racket/base

(require koyo/profiler
         koyo/session
         koyo/url
         net/url
         racket/contract/base
         threading
         web-server/http
         "user.rkt")

;; auth-manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 exn:fail:auth-manager?
 exn:fail:auth-manager:unverified?

 (contract-out
  [current-user (parameter/c (or/c #f user?))]
  [make-auth-manager (-> session-manager? user-manager? auth-manager?)]
  [auth-manager? (-> any/c boolean?)]
  [auth-manager-login! (-> auth-manager? string? string? (or/c #f user?))]
  [auth-manager-logout! (-> auth-manager? void?)]
  [wrap-auth-required (-> auth-manager?
                          (-> request? (listof symbol?))
                          (-> (-> request? response?)
                              (-> request? response?)))]))

(define session-key 'uid)

(define current-user
  (make-parameter #f))

(struct exn:fail:auth-manager exn:fail ())
(struct exn:fail:auth-manager:unverified exn:fail:auth-manager ())

(struct auth-manager (sessions users)
  #:transparent)

(define (make-auth-manager sessions users)
  (auth-manager sessions users))

(define (auth-manager-login! am username password)
  (cond
    [(user-manager-login (auth-manager-users am) username password)
     => (lambda (u)
          (unless (user-verified? u)
            (raise (exn:fail:auth-manager:unverified "this user is not verified" (current-continuation-marks))))

          (begin0 u
            (session-manager-set! (auth-manager-sessions am) session-key (number->string (user-id u)))))]

    [else #f]))

(define (auth-manager-logout! _am)
  (session-remove! session-key))

(define (((wrap-auth-required am req-roles) handler) req)
  (with-timing 'auth "wrap-auth-required"
    (define roles (req-roles req))
    (cond
      [(null? roles)
       (handler req)]

      ;; NOTE: Roles are not actually checked beyond this point.  If you
      ;; implement roles other than 'user then you're going to want to
      ;; change this part of the code.
      [(and~>> (session-manager-ref (auth-manager-sessions am) session-key #f)
               (string->number)
               (user-manager-lookup/id (auth-manager-users am)))
       => (lambda (user)
            (parameterize ([current-user user])
              (handler req)))]

      [else
       (redirect-to (reverse-uri 'login-page #:query `((return . ,(url->string (request-uri req))))))])))
