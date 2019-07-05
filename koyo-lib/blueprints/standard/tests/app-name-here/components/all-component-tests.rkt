#lang racket/base

(require rackunit
         "auth.rkt"
         "hash.rkt"
         "mail.rkt"
         "user.rkt")

(provide
 component-tests)

(define component-tests
  (test-suite
   "component"

   auth-tests
   hash-tests
   mail-tests
   user-tests))
