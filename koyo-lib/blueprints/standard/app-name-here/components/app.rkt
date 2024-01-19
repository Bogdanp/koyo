#lang racket/base

(require koyo
         koyo/database/migrator
         racket/contract/base
         racket/contract/region
         racket/list
         threading
         web-server/dispatch
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/managers/lru
         web-server/servlet-dispatch
         "../pages/all.rkt"
         "auth.rkt"
         "mail.rkt"
         "user.rkt")

(provide
 make-app
 app?
 app-dispatcher)

(struct app (dispatcher)
  #:transparent)

(define/contract (make-app auth broker flashes mailer _migrator sessions users
                           #:debug? [debug? #f]
                           #:memory-threshold [memory-threshold (* 1 1024 1024 1024)]
                           #:static-path [static-path #f])
  (->* [auth-manager? broker? flash-manager? mailer? migrator? session-manager? user-manager?]
       [#:debug? boolean?
        #:memory-threshold exact-positive-integer?
        #:static-path (or/c #f path-string?)]
       app?)
  (define-values (dispatch reverse-uri req-roles)
    (dispatch-rules+roles
     [("")
      #:roles (user)
      dashboard-page]

     [("login")
      (login-page auth)]

     [("logout")
      (logout-page auth)]

     [("password-reset")
      (request-password-reset-page mailer users)]

     [("password-reset" (integer-arg) (string-arg))
      (password-reset-page users)]

     [("signup")
      (signup-page auth mailer users)]

     [("verify" (integer-arg) (string-arg))
      (verify-page users)]))

  (define ((wrap-params handler) req)
    (parameterize ([current-broker broker]
                   [current-continuation-key-cookie-secure? (not debug?)]
                   [current-continuation-wrapper stack]
                   [current-reverse-uri-fn reverse-uri])
      (handler req)))

  ;; Requests go up (starting from the last wrapper) and respones go down!
  (define (stack handler)
    (~> handler
        (wrap-protect-continuations)
        ((wrap-auth-required auth req-roles))
        ((wrap-browser-locale sessions))
        ((wrap-flash flashes))
        ((wrap-session sessions))
        (wrap-preload)
        (wrap-cors)
        (wrap-profiler)
        ((wrap-errors debug?))
        (wrap-params)))

  (define manager
    (make-threshold-LRU-manager (stack expired-page) memory-threshold))

  (define dispatchers
    (list
     (and static-path (make-static-dispatcher static-path))
     (dispatch/servlet #:manager manager (stack dispatch))
     (dispatch/servlet #:manager manager (stack not-found-page))))

  (app (apply sequencer:make (filter-map values dispatchers))))
