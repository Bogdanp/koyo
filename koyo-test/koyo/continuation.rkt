#lang racket/base

(require koyo/continuation
         koyo/testing
         racket/port
         racket/string
         rackunit
         web-server/http
         web-server/managers/manager
         web-server/servlet/servlet-structs)

(provide
 continuation-tests)

(define continuation-tests
 (test-suite
  "continuation"

  (test-suite
   "protect-continuation"

   (test-case "ensures the key in the request matches the current key"
     (define handler
       (protect-continuation
        (lambda (req)
          (response/xexpr '(h1 "Hello")))))

     (check-exn
      exn:fail:servlet-manager:no-instance?
      (lambda ()
        (parameterize ([current-continuation-key "sekrit"])
          (handler (make-test-request)))))

     (check-equal?
      (response-code (parameterize ([current-continuation-key "sekrit"])
                       (handler (make-test-request #:headers (list (make-header #"Cookie" #"_k=sekrit"))))))
      200))

   (test-case "wraps the continuation function inside the current wrapper"
     (parameterize ([current-continuation-wrapper (lambda (hdl)
                                                    (lambda (req)
                                                      (hdl req)
                                                      (response/xexpr '(h1 "Hello from wrapper"))))])
       (define handler-ran? #f)
       (define handler
         (protect-continuation
          (lambda (req)
            (set! handler-ran? #t)
            (response/xexpr '(h1 "Hello")))))

       (define response
        (parameterize ([current-continuation-key "sekrit"])
          (call-with-output-string
           (response-output (handler (make-test-request #:headers '(("Cookie" . "_k=sekrit"))))))))

       (check-equal? response "<h1>Hello from wrapper</h1>")
       (check-true handler-ran?))))

  (test-suite
   "wrap-protect-continuations"

   (test-case "adds a continuation key to the response"
     (define handler
       (wrap-protect-continuations (lambda (req)
                                     (response/xexpr '(h1 "Hello")))))

     (define-values (header value)
       (let ([cookie-header (car (response-headers (handler (make-test-request))))])
         (values (bytes->string/utf-8 (header-field cookie-header))
                 (bytes->string/utf-8 (header-value cookie-header)))))

     (check-equal? header "Set-Cookie")
     (check-true (and (string-contains? value "_k=")
                      (string-contains? value "Path=/;")
                      (string-contains? value "HttpOnly")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests continuation-tests))
