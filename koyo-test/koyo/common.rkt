#lang racket/base

(require db
         koyo
         racket/async-channel
         web-server/web-server)

(provide
 make-test-database
 call-with-web-server)

(define (make-test-database)
  ((make-database-factory
    #:log-statements? #t
    (lambda ()
      (postgresql-connect
       #:server   (or (getenv "KOYO_TEST_DB_HOST") "127.0.0.1")
       #:port     (string->number (or (getenv "KOYO_TEST_DB_PORT") "5432"))
       #:database (or (getenv "KOYO_TEST_DB_NAME") "koyo")
       #:user     (or (getenv "KOYO_TEST_DB_USERNAME") "koyo")
       #:password (or (getenv "KOYO_TEST_DB_PASSWORD") "koyo"))))))

(define (call-with-web-server dispatch proc)
  (define port-ch (make-async-channel))
  (define stop void)
  (define port #f)
  (dynamic-wind
    (lambda ()
      (set! stop (serve
                  #:port 0
                  #:dispatch dispatch
                  #:confirmation-channel port-ch))
      (define port-or-exn
        (sync port-ch))
      (when (exn:fail? port-or-exn)
        (raise port-or-exn))
      (set! port port-or-exn))
    (lambda ()
      (proc port))
    (lambda ()
      (stop))))
