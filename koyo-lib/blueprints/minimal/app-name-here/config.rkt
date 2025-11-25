#lang racket/base

(require koyo/config
         koyo/profiler
         koyo/url
         web-server/http/id-cookie)

(current-option-name-prefix "APP_NAME_HERE")

(define-option version
  #:default "dev")
(define-option debug
  (equal? debug "x"))

(define-option profile
  (begin0 profile
    (profiler-enabled? (and profile #t))))

(define-option log-level
  #:default "info"
  (string->symbol log-level))

(define-option http-host
  #:default "127.0.0.1")
(define-option http-port
  #:default (or (getenv "PORT") "8000")
  (string->number http-port))

(define-option url-scheme
  #:default "http"
  (begin0 url-scheme
    (current-application-url-scheme url-scheme)))

(define-option url-host
  #:default "127.0.0.1"
  (begin0 url-host
    (current-application-url-host url-host)))

(define-option url-port
  #:default (or (getenv "PORT") "8000")
  (begin0 url-port
    (current-application-url-port (string->number url-port))))

(define-option session-cookie-name
  #:default "_sid")
(define-option session-shelf-life
  #:default "86400"
  (string->number session-shelf-life))
(define-option session-secret-key-path
  #:default (build-path (find-system-path 'temp-dir) "app-name-here-secret-key"))
(define-option session-secret-key
  (or session-secret-key (make-secret-salt/file session-secret-key-path)))

(define-option continuation-manager-memory-threshold
  #:default (number->string (* 512 1024 1024))  ;; 512 MiB
  (string->number continuation-manager-memory-threshold))
