#lang racket/base

(require koyo/cors
         koyo/testing
         rackunit
         web-server/http)

(provide
 cors-tests)

(define handler
  (wrap-cors
   (lambda (req)
     (response/xexpr
      #:headers (list (make-header #"X-Test" #"Custom"))
      '(h1 "Hello")))))

(define cors-tests
  (test-suite
   "cors"

   (test-suite
    "wrap-cors"

    (test-case "responds to all OPTIONS requests with a 200 OK and the appropriate CORS headers"
      (define response
        (handler (make-test-request #:method "OPTIONS")))

      (check-equal? (response-code response) 200)
      (check-equal? (response-headers response)
                    (list (make-header #"Content-Length" #"0")
                          (make-header #"Access-Control-Allow-Credentials" #"true")
                          (make-header #"Access-Control-Allow-Origin" #"http://127.0.0.1")
                          (make-header #"Access-Control-Allow-Methods" #"HEAD,DELETE,GET,PATCH,POST,PUT,OPTIONS")
                          (make-header #"Access-Control-Allow-Headers" #"*")
                          (make-header #"Access-Control-Max-Age" #"86400"))))

    (test-case "augments other requests with the appropriate CORS headers"
      (define response
        (handler (make-test-request)))

      (check-equal? (response-code response) 200)
      (check-equal? (response-headers response)
                    (list (make-header #"Access-Control-Allow-Origin" #"http://127.0.0.1")
                          (make-header #"X-Test" #"Custom")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests cors-tests))
