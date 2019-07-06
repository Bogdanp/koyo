#lang racket/base

(require koyo/http
         koyo/testing
         rackunit
         web-server/http)

(provide
 http-tests)

(define http-tests
  (test-suite
   "http"

   (test-suite
    "bindings-ref"

    (test-case "returns #f or a default if the binding is not set"
      (define binds (request-bindings/raw (make-test-request)))
      (check-false (bindings-ref binds 'notset))
      (check-eq? (bindings-ref binds 'notset "a-default") "a-default"))

    (test-case "returns the first value of a binding if it is set"
      (define binds (request-bindings/raw (make-test-request #:query '((a . "1")
                                                                       (a . "2")))))
      (check-equal? (bindings-ref binds 'a) "1")))

   (test-suite
    "bindings-ref-number"

    (test-case "returns the binding as a number when it can be parsed as such"
      (define binds (request-bindings/raw (make-test-request #:query '((a . "1")
                                                                       (b . "nope")))))
      (check-eq? (bindings-ref-number binds 'a) 1)
      (check-false (bindings-ref-number binds 'b))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests http-tests))
