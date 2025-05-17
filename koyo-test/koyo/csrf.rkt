#lang racket/base

(require component
         koyo/csrf
         koyo/session
         koyo/testing
         racket/match
         racket/port
         rackunit
         web-server/http)

(provide csrf-tests)

(define session-cookie-name "_sid")

(define sessions
  (component-start
   ((make-session-manager-factory
     #:cookie-name session-cookie-name
     #:shelf-life 86400
     #:secret-key #"supercalifragilisticexpialidocious"
     #:store (make-memory-session-store)))))

(define wrapper
  (compose1 (wrap-session sessions)
            (wrap-csrf sessions)))

(define csrf-tests
  (test-suite
   "csrf"

   (test-suite
    "request-csrf-token"

    (test-case "returns #f if there is no token in the request"
      (check-false (request-csrf-token (make-test-request))))

    (test-case "extracts CSRF tokens from headers"
      (check-equal?
       (request-csrf-token (make-test-request #:headers (list (make-header #"x-csrf-token" #"a"))))
       "a"))

    (test-case "extracts CSRF tokens from bindings"
      (check-equal?
       (request-csrf-token (make-test-request #:bindings (list (make-binding:form #"csrf-token" #"a"))))
       "a")))

   (test-suite
    "request-protected?"

    (test-case "returns #t when the current request should be protected"
      (check-false (request-protected? (make-test-request)))
      (for ([method '(#"DELETE" #"PATCH" #"POST" #"PUT" #"RANDOM")])
        (check-not-false (request-protected? (make-test-request #:method method))))))

   (test-suite
    "wrap-csrf"

    (test-case "does nothing for GET requests except generate the token"
      ((wrapper
        (lambda (_req)
          (check-not-false (current-csrf-token))
          (response/xexpr '(div))))
       (make-test-request)))

    (test-case "fails the request if it does not contain the expected token"
      (check-equal?
       (response-code
        ((wrapper (λ (_req) (response/xexpr '(div))))
         (make-test-request #:method "POST")))
       403))

    (test-case "fails the request with a custom error handler"
      (parameterize ([current-csrf-error-handler
                      (lambda (_req)
                        (response/output
                         (lambda (out)
                           (display "fail" out))))])
        (define resp
          ((wrapper (λ (_req) (response/xexpr '(div))))
           (make-test-request #:method "POST")))
        (define content
          (call-with-output-string
           (lambda (out)
             ((response-output resp) out))))
        (check-equal? content "fail")))

    (test-case "passes the request if it contains the expected token"
      (define response-1
        ((wrapper (λ (_req) (response/xexpr (current-csrf-token))))
         (make-test-request)))

      (match-define (list _ session-id)
        (regexp-match #px"_sid=([^;]+);" (bytes->string/utf-8 (header-value (car (response-headers response-1))))))

      (define session-id-cookie
        (string->bytes/utf-8 (format "~a=~a" session-cookie-name session-id)))

      (define csrf-token
        (string->bytes/utf-8
         (call-with-output-string (response-output response-1))))

      (define response-2
        ((wrapper (λ (_req) (response/xexpr '(p "ok"))))
         (make-test-request
          #:method "POST"
          #:headers (list (make-header #"cookie" session-id-cookie)
                          (make-header #"x-csrf-token" csrf-token)))))

      (check-equal? (response-code response-2) 200)
      (check-equal? (call-with-output-string (response-output response-2)) "<p>ok</p>")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests csrf-tests))
