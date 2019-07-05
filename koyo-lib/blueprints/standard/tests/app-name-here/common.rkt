#lang racket/base

(require db
         koyo/database
         koyo/session

         app-name-here/components/mail
         app-name-here/components/user
         (prefix-in config: app-name-here/config))


;; database ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-test-database)

(define (make-test-database)
  ((make-database-factory (lambda ()
                            (postgresql-connect #:database config:test-db-name
                                                #:user     config:test-db-username
                                                #:password config:test-db-password)))))


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
