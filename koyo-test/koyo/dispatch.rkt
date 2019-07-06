#lang racket/base

(require koyo/dispatch
         koyo/testing
         racket/function
         rackunit
         web-server/dispatch)

(provide
 dispatch-tests)

(define (home-page req)
  'home)

(define ((order-page orders) req id)
  'order)

(define ((admin:edit-order-page orders) req id)
  'edit-order)

(define dispatch-tests
  (test-suite
   "dispatch"

   (test-suite
    "dispatch-rules+roles"

    (test-case "generates a function from requests to lists of roles"
      (define-values (dispatch _ request-roles)
        (dispatch-rules+roles
         [("")
          home-page]

         [("orders" (integer-arg))
          #:roles (user)
          (order-page 'order-manager)]

         [("admin" "orders" (integer-arg))
          #:roles (admin)
          (admin:edit-order-page 'order-manager)]))

      (check-equal? (request-roles (make-test-request)) '())
      (check-equal? (request-roles (make-test-request #:path "/invalid")) '())
      (check-equal? (request-roles (make-test-request #:path "/orders/1")) '(user))
      (check-equal? (request-roles (make-test-request #:path "/orders/1" #:method "POST")) '())
      (check-equal? (request-roles (make-test-request #:path "/admin/orders/1")) '(admin)))

    (test-case "generates a reverse-uri function based upon symbols"
      (define-values (dispatch reverse-uri _)
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
      (check-equal? (reverse-uri 'edit-order-page 1) "/admin/orders/1")))))


(module+ test
  (require rackunit/text-ui)
  (run-tests dispatch-tests))
