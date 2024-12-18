#lang racket/base

(require koyo/dispatch
         koyo/testing
         koyo/url
         (prefix-in http: net/http-easy)
         rackunit
         web-server/dispatch
         web-server/dispatchers/dispatch
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/http
         web-server/servlet-dispatch
         "common.rkt")

(provide
 dispatch-tests)

(define (home-page _req)
  'home)

(define ((order-page _orders) _req _id)
  'order)

(define ((admin:edit-order-page _orders) _req _id)
  'edit-order)

(define dispatch-tests
  (test-suite
   "dispatch"

   (test-suite
    "dispatch-rules+roles"

    (test-case "generates a function from requests to lists of roles"
      (define-values (_dispatch _reverse-uri req-roles)
        (dispatch-rules+roles
         [("")
          home-page]

         [("orders" (integer-arg))
          #:roles (user)
          (order-page 'order-manager)]

         [("admin" "orders" (integer-arg))
          #:roles (admin)
          (admin:edit-order-page 'order-manager)]))

      (check-equal? (req-roles (make-test-request)) '())
      (check-equal? (req-roles (make-test-request #:path "/invalid")) '())
      (check-equal? (req-roles (make-test-request #:path "/orders/1")) '(user))
      (check-equal? (req-roles (make-test-request #:path "/orders/1" #:method "POST")) '())
      (check-equal? (req-roles (make-test-request #:path "/admin/orders/1")) '(admin)))

    (test-case "generates a reverse-uri function based upon symbols"
      (define-values (_dispatch reverse-uri _req-roles)
        (dispatch-rules+roles
         [("")
          home-page]

         [("orders" (integer-arg))
          (order-page 'order-manager)]

         [("admin" "orders" (integer-arg))
          #:name 'edit-order-page
          (admin:edit-order-page 'order-manager)]))

      (check-equal? (reverse-uri 'home-page) "/")
      (check-equal? (reverse-uri 'order-page 1) "/orders/1")
      (check-equal? (reverse-uri 'edit-order-page 1) "/admin/orders/1"))

    (test-case "calls next-dispatcher if no rule matches"
      (define-values (dispatch _reverse-uri _req-roles)
        (dispatch-rules+roles
         [("")
          home-page]

         [("orders" (integer-arg))
          (order-page 'order-manager)]))

      (check-not-exn
       (lambda ()
         (dispatch (make-test-request #:path "/"))))

      (check-not-exn
       (lambda ()
         (dispatch (make-test-request #:path "/orders/1"))))

      (check-exn
       exn:dispatcher?
       (lambda ()
         (dispatch (make-test-request #:path "/invalid"))))))

   (test-suite
    "dispatch/mount"

    (test-case "mounting + reverse-uri"
      (define jobs null)
      (define-values (dispatch/inner reverse-uri/inner _req-roles/inner)
        (dispatch-rules+roles
         [("")
          #:name 'jobs
          (lambda (_req)
            (response/jsexpr
             (for/list ([(job idx) (in-indexed (in-list jobs))])
               (hasheq 'job job 'link (reverse-uri 'get-job idx)))))]
         [("")
          #:method "post"
          #:name 'add-job
          (lambda (_req)
            (set! jobs (cons "a job" jobs))
            (response/empty))]
         [((integer-arg))
          #:name 'get-job
          (lambda (_req idx)
            (response/jsexpr (list-ref jobs idx)))]))
      (define-values (dispatch _reverse-uri/outer _req-roles)
        (dispatch-rules+roles
         [("")
          #:name 'home
          (lambda (_req)
            (response/xexpr '(h1 "Home")))]))
      (call-with-web-server
       (sequencer:make
        (dispatch/servlet dispatch)
        (dispatch/mount
         "/api/v1/jobs"
         (dispatch/servlet
          (lambda (req)
            (parameterize ([current-reverse-uri-fn reverse-uri/inner])
              (dispatch/inner req))))))
       (lambda (port)
         (define (make-endpoint [path ""])
           (format "http://127.0.0.1:~a/~a" port path))
         (check-equal?
          (http:response-xexpr
           (http:get (make-endpoint)))
          `(h1 () "Home"))
         (check-equal?
          (http:response-json
           (http:get (make-endpoint "api/v1/jobs")))
          '())
         (check-equal?
          (http:response-status-code
           (http:post (make-endpoint "api/v1/jobs")))
          204)
         (check-equal?
          (http:response-json
           (http:get (make-endpoint "api/v1/jobs")))
          (list
           (hasheq 'job "a job" 'link "/api/v1/jobs/0")))
         (check-equal?
          (http:response-json
           (http:get (make-endpoint "api/v1/jobs/0")))
          "a job")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests dispatch-tests))
