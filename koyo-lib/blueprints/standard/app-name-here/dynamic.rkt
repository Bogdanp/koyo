#lang racket/base

(require (for-syntax racket/base)
         component
         db
         koyo/database
         koyo/database/migrator
         koyo/flash
         koyo/hasher
         koyo/logging
         koyo/mail/postmark
         koyo/server
         koyo/session
         racket/contract
         racket/runtime-path
         "components/app.rkt"
         "components/auth.rkt"
         "components/mail.rkt"
         "components/user.rkt"
         (prefix-in config: "config.rkt"))

;; System ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path migrations-path
  (build-path 'up "migrations"))

(define mail-adapter
  (if config:postmark-token
      (make-postmark-mail-adapter (postmark config:postmark-token))
      (make-stub-mail-adapter)))

(define-system prod
  [app (auth flashes mailer migrator sessions users) make-app]
  [auth (sessions users) make-auth-manager]
  [db (make-database-factory (lambda ()
                               (postgresql-connect #:database config:db-name
                                                   #:user     config:db-username
                                                   #:password config:db-password
                                                   #:server   config:db-host
                                                   #:port     config:db-port)))]
  [flashes (sessions) make-flash-manager]
  [hasher (make-argon2id-hasher-factory
           #:parallelism 2
           #:iterations 256
           #:memory 2048)]
  [mailer (make-mailer-factory #:adapter mail-adapter
                               #:sender config:support-email
                               #:common-variables config:common-mail-variables)]
  [migrator (db) (make-migrator-factory migrations-path)]
  [server (app) (compose1
                 (make-server-factory #:host config:http-host
                                      #:port config:http-port)
                 app-dispatcher)]
  [sessions (make-session-manager-factory #:cookie-name config:session-cookie-name
                                          #:cookie-secure? #f
                                          #:cookie-same-site 'lax
                                          #:shelf-life config:session-shelf-life
                                          #:secret-key config:session-secret-key
                                          #:store (make-memory-session-store #:file-path "/tmp/app-name-here-session.rktd"))]
  [users (db hasher) make-user-manager])


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
                (north-adapter        . ,config:log-level)
                (server               . ,config:log-level)
                (session              . ,config:log-level)
                (system               . ,config:log-level))))

  (current-system prod-system)
  (system-start prod-system)

  (lambda ()
    (system-stop prod-system)
    (stop-logger)))


(module+ main
  (define stop (start))
  (with-handlers ([exn:break?
                   (lambda (_)
                     (stop))])
    (sync/enable-break never-evt)))
