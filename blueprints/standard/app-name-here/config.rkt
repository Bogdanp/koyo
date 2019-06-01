#lang racket/base

(require koyo/config
         koyo/profiler
         koyo/url
         web-server/http/id-cookie)

(current-option-name-prefix "APP_NAME_HERE")

(define-option version #:default "dev")
(define-option debug
  (equal? debug "x"))

(define-option profile
  (begin0 profile
    (profiler-enabled? (and profile #t))))

(define-option log-level #:default "info"
  (string->symbol log-level))

(define-option http-host #:default "127.0.0.1")
(define-option http-port #:default "8000"
  (string->number http-port))

(define-option url-scheme #:default "http"
  (begin0 url-scheme
    (current-application-url-scheme url-scheme)))

(define-option url-host #:default "127.0.0.1"
  (begin0 url-host
    (current-application-url-host url-host)))

(define-option url-port #:default "8000"
  (begin0 url-port
    (current-application-url-port (string->number url-port))))

(define-option db-name #:default "app_name_here")
(define-option db-username #:default "app_name_here")
(define-option db-password #:default "app_name_here")
(define-option db-host #:default "127.0.0.1")
(define-option db-port #:default "5432"
  (string->number db-port))

(define-option test-db-name #:default "app_name_here_tests")
(define-option test-db-username #:default "app_name_here")
(define-option test-db-password #:default "app_name_here")
(define-option test-db-host #:default "127.0.0.1")
(define-option test-db-port #:default "5432"
  (string->number test-db-port))

(define-option session-cookie-name #:default "_sid")
(define-option session-shelf-life #:default "86400"
  (string->number session-shelf-life))
(define-option session-secret-key-path #:default "/tmp/app-name-here-secret-key")
(define-option session-secret-key
  (or session-secret-key (make-secret-salt/file session-secret-key-path)))

(define-option postmark-token)

(define-option product-name "AppNameHere")
(define-option company-name "AppNameHere")
(define-option company-address "")
(define-option support-name "Bot Botterson")
(define-option support-email "support@app-name-here.com")

(provide common-mail-variables)
(define common-mail-variables

  (hasheq 'product_url     (make-application-url)
          'product_name    product-name
          'company_name    company-name
          'company_address company-address
          'sender_name     support-name
          'support_email   support-email))
