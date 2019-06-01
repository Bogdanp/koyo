#lang racket/base

(require (for-syntax racket/base)
         component
         koyo/database
         koyo/flash
         koyo/l10n
         koyo/logging
         koyo/server
         koyo/session
         koyo/url
         postmark
         racket/contract
         racket/runtime-path
         "components/app.rkt"
         "components/auth.rkt"
         "components/mail.rkt"
         "components/user.rkt"
         (prefix-in config: "config.rkt"))


;; System ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path locales-path
  (build-path 'up "resources" "locales"))

(current-locale-specifier 'app-name-here)
(load-locales! locales-path)

(define mail-adapter
  (if config:postmark-token
      (make-postmark-mail-adapter (postmark config:postmark-token))
      (make-stub-mail-adapter)))

(define-system prod
  [app (auth db flashes mailer sessions users) make-app]
  [auth (sessions users) make-auth-manager]
  [db (make-database-factory #:database config:db-name
                             #:username config:db-username
                             #:password config:db-password
                             #:host config:db-host
                             #:port config:db-port)]
  [flashes (sessions) make-flash-manager]
  [mailer (make-mailer #:adapter mail-adapter
                       #:sender config:support-email
                       #:common-variables config:common-mail-variables)]
  [server (app) (compose1 (make-server-factory #:host config:http-host
                                               #:port config:http-port) app-dispatcher)]
  [sessions (make-session-manager-factory #:cookie-name config:session-cookie-name
                                          #:cookie-secure? #f
                                          #:shelf-life config:session-shelf-life
                                          #:secret-key config:session-secret-key
                                          #:store (make-memory-session-store #:file-path "/tmp/app-name-here-session.ss"))]
  [users (db) user-manager])


;; Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 prod-system
 start)

(define/contract (start)
  (-> (-> void?))

  (define stop-logger
    (start-logger
     #:levels `((app                  . ,config:log-level)
                (mail-adapter         . ,config:log-level)
                (memory-session-store . ,config:log-level)
                (server               . ,config:log-level)
                (session              . ,config:log-level)
                (system               . ,config:log-level))))

  (system-start prod-system)

  (lambda ()
    (system-stop prod-system)
    (stop-logger)))


(module+ main
  (define stop (start))
  (with-handlers ([exn:break? (lambda _
                                (stop))])
    (sync/enable-break never-evt)))
