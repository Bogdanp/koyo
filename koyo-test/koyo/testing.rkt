#lang racket/base

(require koyo/testing
         rackunit
         web-server/http)

(provide testing-tests)

(define testing-tests
  (test-suite
   "testing"

   (test-suite
    "make-test-request"

    (test-case "appends query params to bindings"
      (define req
        (make-test-request #:bindings (list (make-binding:form #"a" #"a"))
                           #:query '((b . "b"))))

      (check-equal? (request-bindings/raw req)
                    (list (make-binding:form #"a" #"a")
                          (make-binding:form #"b" #"b"))))

    (test-case "converts headers expressed as pairs to header values"
      (define req
        (make-test-request #:headers (list (make-header #"host" #"localhost")
                                           '("x-forwarded-for" . "127.0.0.1"))))

      (check-equal? (request-headers/raw req)
                    (list (make-header #"host" #"localhost")
                          (make-header #"x-forwarded-for" #"127.0.0.1")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests testing-tests))
