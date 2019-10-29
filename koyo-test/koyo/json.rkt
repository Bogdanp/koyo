#lang at-exp racket/base

(require koyo/json
         racket/format
         racket/port
         rackunit
         web-server/http)

(provide
 json-tests)

(define json-tests
  (test-suite
   "json"

   (test-suite
    "response/json"

    (test-case "produces JSON responses"
      (define a-response
        (response/json
         #:code 201
         #:headers (list (make-header #"x-some-header" #"val"))
         (hasheq 'status "created")))

      (check-equal? (response-code a-response) 201)
      (check-equal? (response-message a-response) #"Created")
      (check-equal? (call-with-output-string (response-output a-response)) @~a{{"status":"created"}})))))

(module+ test
  (require rackunit/text-ui)
  (run-tests json-tests))
