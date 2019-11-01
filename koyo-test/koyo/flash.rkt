#lang racket/base

(require component
         koyo/flash
         (submod koyo/flash private)
         koyo/session
         rackunit)

(provide flash-tests)

(define-system test
  [flashes (sessions) make-flash-manager]
  [sessions (make-session-manager-factory #:cookie-name "session-id"
                                          #:shelf-life 86400
                                          #:secret-key #"supercalifragilistcexpialidocious"
                                          #:store (make-memory-session-store))])

(define flash-tests
  (test-suite
   "flash"
   #:before
   (lambda _
     (system-start test-system))

   #:after
   (lambda _
     (system-stop test-system))

   (test-suite
    "flash"

    (test-case "can add messages"
      (parameterize ([current-session-id "a-session-id"])
        (flash (system-get test-system 'flashes) 'error "Error message 1.")
        (flash (system-get test-system 'flashes) 'info "Info message 1.")
        (flash (system-get test-system 'flashes) 'info "Info message 2.")

        (check-equal?
         (session-manager-ref (system-get test-system 'sessions) 'flash.messages)
         '((info . "Info message 2.")
           (info . "Info message 1.")
           (error . "Error message 1.")))))

    (test-case "can add messages using the implicit argument"
      (parameterize ([current-flash-manager (system-get test-system 'flashes)]
                     [current-session-id "another-session-id"])
        (flash 'error "Error message 1.")
        (flash 'info "Info message 1.")
        (flash 'info "Info message 2.")

        (check-equal?
         (session-manager-ref (system-get test-system 'sessions) 'flash.messages)
         '((info . "Info message 2.")
           (info . "Info message 1.")
           (error . "Error message 1."))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests flash-tests))
