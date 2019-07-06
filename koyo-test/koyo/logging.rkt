#lang racket/base

(require koyo/logging
         racket/file
         racket/port
         rackunit)

(provide logging-tests)

(define logging-tests
  (test-suite
   "logging"

   (test-suite
    "start-logger"

    (test-case "logs to a file until stopped"
      (define filename (make-temporary-file))
      (call-with-output-file filename
        #:exists 'replace
        (lambda (out)
          (define stopper (start-logger
                           #:levels '((foo . debug)
                                      (bar . info))
                           #:output-port out))

          (define-logger foo)
          (define-logger bar)
          (log-foo-debug "log 1")
          (log-bar-debug "log 2")
          (log-foo-debug "log 3")
          (stopper)))

      (define lines (call-with-input-file filename port->lines))
      (check-equal? (length lines) 2)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests logging-tests))
