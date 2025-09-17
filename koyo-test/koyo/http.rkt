#lang racket/base

(require koyo/http
         koyo/testing
         net/url
         rackunit
         web-server/http)

(provide
 http-tests)

(define http-tests
  (test-suite
   "http"

   (test-suite
    "request-reroot"

    (test-case "null root"
      (check-equal?
       (url->string
        (request-uri
         (request-reroot
          (make-test-request #:path "/a/b/c")
          (string->url ""))))
       "http://127.0.0.1:80/a/b/c"))

    (test-case "rebased root"
      (check-equal?
       (url->string
        (request-uri
         (request-reroot
          (make-test-request #:path "/a/b/c")
          (string->url "/a/b"))))
       "http://127.0.0.1:80/c"))

    (test-case "no match"
      (check-false
       (request-reroot
        (make-test-request #:path "/d")
        (string->url "/a/b")))))

   (test-suite
    "request-path"

    (test-case "returns the correct path for a request"
      (check-equal? (request-path (make-test-request)) "/")
      (check-equal? (request-path (make-test-request #:path "/a/b/c")) "/a/b/c")
      (check-equal? (request-path (make-test-request #:path "/a/b/c;test")) "/a/b/c")))

   (test-suite
    "request-ip-address"

    (check-equal?
     (request-ip-address
      (make-test-request))
     "127.0.0.1")
    (check-equal?
     (request-ip-address
      (make-test-request
       #:headers `(("x-real-ip" . "10.0.0.1"))))
     "10.0.0.1")
    (check-equal?
     (request-ip-address
      (make-test-request
       #:headers `(("x-forwarded-for" . "10.0.0.2,10.0.0.1")
                   ("x-real-ip" . "10.0.0.1"))))
     "10.0.0.2"))

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
