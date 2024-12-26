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
         "user.rkt"
         "workspace-member.rkt"
         "workspace.rkt")

(provide
 make-app
 app?
 app-dispatcher)

(struct app (dispatcher)
  #:transparent)

(define/contract (make-app auth broker broker-admin db flashes mailer _migrator sessions users
                           #:debug? [debug? #f]
                           #:memory-threshold [memory-threshold (* 1 1024 1024 1024)]
                           #:static-path [static-path #f])
  (->* [auth-manager? broker? broker-admin/c database? flash-manager? mailer? migrator? session-manager? user-manager?]
       [#:debug? boolean?
        #:memory-threshold exact-positive-integer?
        #:static-path (or/c #f path-string?)]
       app?)
  (define-values (dispatch reverse-uri req-roles)
    (let ([wrap-workspace (wrap-workspace db)])
      (dispatch-rules+roles
       [("api" "v1" "password-reset-tokens")
        #:method "post"
        (create-password-reset-token-page db mailer users)]
       [("api" "v1" "sessions")
        #:method "post"
        (login-page auth)]
       [("api" "v1" "users" "me")
        #:roles (user)
        (me-page users)]
       [("api" "v1" "users")
        #:method "post"
        (create-user-page db mailer users)]
       [("api" "v1" "users" (integer-arg) "password")
        #:method "patch"
        (reset-password-page users)]
       [("api" "v1" "workspaces")
        #:roles (user)
        (workspaces-page db)]
       [("api" "v1" "workspaces")
        #:method "post"
        #:roles (user)
        (create-workspace-page db)]
       [("api" "v1" "workspaces" (integer-arg) "invites")
        #:method "post"
        #:roles (user)
        #:name 'invite-workspace-member-page
        (wrap-workspace
         (wrap-permissions
          #:permissions '(write)
          (invite-workspace-member-page db mailer)))]
       [("api" "v1" "workspaces" (integer-arg) "invites" (string-arg))
        #:roles (user)
        (workspace-invite-page db)]
       [("api" "v1" "workspaces" (integer-arg) "invites" (string-arg) "join")
        #:method "post"
        #:roles (user)
        (join-workspace-page db)]
       [("api" "v1" "workspaces" (integer-arg) "invites" (integer-arg))
        #:method "delete"
        #:roles (user)
        #:name 'delete-workspace-invite-page
        (wrap-workspace
         (wrap-permissions
          #:permissions '(write)
          (delete-workspace-invite-page db)))]
       [("api" "v1" "workspaces" (integer-arg) "members")
        #:roles (user)
        #:name 'workspace-members-page
        (wrap-workspace
         (wrap-permissions
          (workspace-members-page db)))]
       [("api" "v1" "workspaces" (integer-arg) "members" (integer-arg))
        #:method "delete"
        #:roles (user)
        #:name 'delete-workspace-member-page
        (wrap-workspace
         (wrap-permissions
          #:permissions '(write)
          (delete-workspace-member-page db)))]
       [("logout")
        (logout-page auth)]
       [("verify" (integer-arg) (string-arg))
        (verify-user-page users)]
       [else app-page])))

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
        ((wrap-auth auth req-roles))
        ((wrap-browser-locale sessions))
        ((wrap-flash flashes))
        ((wrap-session sessions))
        (wrap-preload)
        (wrap-cors)
        (wrap-profiler)
        ((wrap-errors debug?))
        (wrap-params)))

  (define broker-admin-servlet
    (dispatch/servlet
     (~> broker-admin
         ((wrap-auth auth (λ (_) '(admin))))
         ((wrap-session sessions)))))

  (define manager
    (make-threshold-LRU-manager (stack expired-page) memory-threshold))

  (define dispatchers
    (list
     (and static-path (make-static-dispatcher static-path))
     (dispatch/mount "/_koyo/jobs" broker-admin-servlet)
     (dispatch/servlet #:manager manager (stack dispatch))))

  (app (apply sequencer:make (filter-map values dispatchers))))
