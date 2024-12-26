#lang racket/base

(require koyo/database
         koyo/profiler
         koyo/session
         koyo/url
         net/url
         racket/contract/base
         racket/match
         threading
         web-server/http
         "user.rkt")

;; auth-manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 exn:fail:auth-manager?
 exn:fail:auth-manager:unverified?
 (contract-out
  [current-user (parameter/c (or/c #f user?))]
  [current-user-id (-> id/c)]
  [make-auth-manager
   (-> session-manager? user-manager? auth-manager?)]
  [auth-manager?
   (-> any/c boolean?)]
  [login!
   (-> auth-manager? string? string? (or/c #f user?))]
  [logout!
   (-> auth-manager? void?)]
  [wrap-auth
   (-> auth-manager?
       (-> request? (listof symbol?))
       (-> (-> request? response?)
           (-> request? response?)))]))

(define key 'uid)

(define current-user
  (make-parameter #f))
(define current-user-id
  (compose1 user-id current-user))

(struct exn:fail:auth-manager exn:fail ())
(struct exn:fail:auth-manager:unverified exn:fail:auth-manager ())

(define (raise-unverified-error)
  (raise (exn:fail:auth-manager:unverified "this user is not verified" (current-continuation-marks))))

(struct auth-manager (sessions users)
  #:transparent)

(define (make-auth-manager sessions users)
  (auth-manager sessions users))

(define (login! am username password)
  (with-timing 'auth "login!"
    (match-define (auth-manager sm um) am)
    (cond
      [(login um username password)
       => (lambda (u)
            (begin0 u
              (unless (user-verified? u)
                (raise-unverified-error))
              (session-manager-set! sm key (number->string (user-id u)))))]
      [else #f])))

(define (logout! am)
  (with-timing 'auth "logout!"
    (match-define (auth-manager sm _) am)
    (session-manager-remove! sm key)))

(define (((wrap-auth am req-roles) handler) req)
  (with-timing 'auth "wrap-auth"
    (match-define (auth-manager sm _) am)
    (define roles (req-roles req))
    (define maybe-user
      (and~>
       (session-manager-ref sm key #f)
       (string->number)
       (lookup-user-by-id (auth-manager-users am) _)))
    (cond
      [(null? roles)
       (handler req)]
      [(and maybe-user (user-has-roles? maybe-user roles))
       (parameterize ([current-user maybe-user])
         (handler req))]
      [else
       (response/xexpr
        #:code 403
        '(h1 "Forbidden"))])))
