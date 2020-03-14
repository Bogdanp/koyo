#lang racket/base

(require (for-syntax racket/base)
         db
         koyo/database
         koyo/database/migrator
         koyo/session
         racket/runtime-path
         rackunit/text-ui

         app-name-here/components/mail
         app-name-here/components/user
         (prefix-in config: app-name-here/config))


;; database ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-test-database
 make-test-migrator
 run-db-tests)

(define-runtime-path migrations-path
  (build-path 'up 'up "migrations"))

(define (make-test-database-connection)
  (postgresql-connect
   #:server   config:test-db-host
   #:port     config:test-db-port
   #:database config:test-db-name
   #:user     config:test-db-username
   #:password config:test-db-password))

(define (make-test-database)
  ((make-database-factory make-test-database-connection)))

(define make-test-migrator
  (make-migrator-factory migrations-path))

(define (run-db-tests test [verbosity 'normal])
  (define conn (make-test-database-connection))
  (query-exec conn "create table if not exists __test_mutex()")
  (call-with-transaction conn
    #:isolation 'serializable
    (lambda _
      (query-exec conn "lock table __test_mutex")
      (run-tests test verbosity))))


;; mail ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-test-mailer)

(define (make-test-mailer)
  ((make-mailer-factory #:adapter (make-stub-mail-adapter)
                        #:sender config:support-email
                        #:common-variables config:common-mail-variables)))


;; sessions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-test-session-manager)

(define (make-test-session-manager)
  ((make-session-manager-factory #:cookie-name config:session-cookie-name
                                 #:shelf-life config:session-shelf-life
                                 #:secret-key config:session-secret-key
                                 #:store (make-memory-session-store))))

;; user ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-test-user!)

(define (make-test-user! users
                         [username "bogdan@example.com"]
                         [password "hunter2"])
  (user-manager-create! users username password))
