#lang racket/base

(require (for-syntax racket/base)
         component
         koyo/continuation
         koyo/cors
         koyo/database/migrator
         koyo/dispatch
         koyo/flash
         koyo/l10n
         koyo/mime
         koyo/preload
         koyo/profiler
         koyo/session
         koyo/url
         net/url
         racket/contract
         racket/runtime-path
         threading
         web-server/dispatch
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/dispatchers/filesystem-map
         web-server/managers/lru
         web-server/servlet-dispatch
         (prefix-in config: "../config.rkt")
         "../pages/all.rkt"
         "auth.rkt"
         "mail.rkt"
         "user.rkt")

(provide
 make-app
 app?
 app-dispatcher)

(define-runtime-path static-path
  (build-path 'up 'up "static"))

(define url->path
  (make-url->path static-path))

(define (static-url->path u)
  (url->path (struct-copy url u [path (cdr (url-path u))])))

(define static-dispatcher
  (files:make
   #:url->path static-url->path
   #:path->mime-type path->mime-type))

(struct app (dispatcher)
  #:methods gen:component [])

(define/contract (make-app auth flashes mailer _migrator sessions users)
  (-> auth-manager? flash-manager? mailer? migrator? session-manager? user-manager? app?)
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
      (request-password-reset-page flashes mailer users)]

     [("password-reset" (integer-arg) (string-arg))
      (password-reset-page flashes mailer users)]

     [("signup")
      (signup-page auth mailer users)]

     [("verify" (integer-arg) (string-arg))
      (verify-page flashes users)]))

  ;; Requests go up (starting from the last wrapper) and respones go down!
  (define (stack handler)
    (~> handler
        ((wrap-auth-required auth req-roles))
        ((wrap-browser-locale sessions))
        ((wrap-flash flashes))
        ((wrap-session sessions))
        (wrap-protect-continuations)
        (wrap-preload)
        (wrap-cors)
        (wrap-profiler)))

  (when config:debug
    (current-continuation-key-cookie-secure? #f))
  (current-continuation-wrapper stack)
  (current-reverse-uri-fn reverse-uri)

  (define manager
    (make-threshold-LRU-manager (stack expired-page) (* 1024 1024 512)))

  (app (sequencer:make
        (filter:make #rx"^/static/.+$" static-dispatcher)
        (dispatch/servlet #:manager manager (stack dispatch))
        (dispatch/servlet #:manager manager (stack not-found-page)))))
