#lang racket/base

(require component
         koyo/session
         racket/file
         racket/string
         rackunit)

(provide session-tests)

(define session-manager
  ((make-session-manager-factory
    #:cookie-name "session-id"
    #:shelf-life 86400
    #:secret-key #"supercalifragilisticexpialidocious"
    #:store (make-memory-session-store))))

(define session-tests
  (test-suite
   "session"

   (test-suite
    "session-store"

    (test-suite
     "memory-session-store"

     (test-case "can generate unique ids"
       (define ss (make-memory-session-store))
       (check-true (string-prefix? (session-store-generate-id! ss) "1."))
       (check-true (string-prefix? (session-store-generate-id! ss) "2.")))

     (test-case "can store and retrieve session data"
       (define ss (make-memory-session-store))
       (session-store-set! ss "session-1" 'a 42)
       (check-equal? (session-store-ref ss "session-1" 'a #f) 42)
       (check-false (session-store-ref ss "session-2" 'a #f))

       (session-store-remove! ss "session-1" 'a)
       (check-false (session-store-ref ss "session-1" 'a #f))

       (session-store-update! ss "session-1" 'a add1 0)
       (check-equal? (session-store-ref ss "session-1" 'a #f) 1)

       (session-store-update! ss "session-1" 'a add1 0)
       (check-equal? (session-store-ref ss "session-1" 'a #f) 2))

     (test-case "can persist and load its contents to/from disk"
       (define output-file (make-temporary-file))
       (define ss1 (make-memory-session-store #:file-path output-file))
       (session-store-set! ss1 "session-1" 'a 42)
       (session-store-persist! ss1)

       (define ss2 (make-memory-session-store #:file-path output-file))
       (session-store-load! ss2)
       (check-equal? (session-store-ref ss2 "session-1" 'a #f) 42))

     (test-case "garbage-collects stale sessions"
       (define ss (make-memory-session-store #:ttl 1))
       (session-store-set! ss "session-1" 'a 42)
       (check-equal? (session-store-ref ss "session-1" 'a #f) 42)

       (sleep 1.1)
       (sync (system-idle-evt))
       (check-false (session-store-ref ss "session-1" 'a #f))
       (session-store-set! ss "session-1" 'a 1))))

   ;; TODO: Test wrap-session.
   (test-suite
    "session-manager"
    #:before
    (lambda ()
      (component-start session-manager))

    #:after
    (lambda ()
      (component-stop session-manager))

    (parameterize ([current-session-id "a"])
      (session-manager-set! session-manager 'uid "1")

      (test-case "can ref keys"
        (check-equal? "1" (session-manager-ref session-manager 'uid)))

      (test-case "can ref missing keys w/ error"
        (check-exn
         exn:fail:user?
         (lambda ()
           (session-manager-ref session-manager 'missing))))

      (test-case "can ref missing keys w/ default"
        (check-false (session-manager-ref session-manager 'missing #f)))

      (test-case "can't ref other people's keys"
        (parameterize ([current-session-id "b"])
          (check-false (session-manager-ref session-manager 'uid #f))))

      (test-case "can't ref removed keys"
        (session-manager-remove! session-manager 'uid)
        (check-false (session-manager-ref session-manager 'uid #f)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests session-tests))
