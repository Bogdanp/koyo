#lang racket/base

(require component
         koyo/logging
         koyo/server
         koyo/session
         racket/contract
         "components/app.rkt"
         (prefix-in config: "config.rkt"))

;; System ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-system prod
  [app (sessions) make-app]
  [server (app) (compose1
                 (make-server-factory #:host config:http-host
                                      #:port config:http-port)
                 app-dispatcher)]
  [sessions (make-session-manager-factory #:cookie-name config:session-cookie-name
                                          #:cookie-secure? #f
                                          #:cookie-same-site 'lax
                                          #:shelf-life config:session-shelf-life
                                          #:secret-key config:session-secret-key
                                          #:store (make-memory-session-store #:file-path "/tmp/app-name-here-session.rktd"))])


;; Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 prod-system
 start)

(define/contract (start)
  (-> (-> void?))

  (define stop-logger
    (start-logger
     #:levels `((app                  . ,config:log-level)
                (memory-session-store . ,config:log-level)
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
