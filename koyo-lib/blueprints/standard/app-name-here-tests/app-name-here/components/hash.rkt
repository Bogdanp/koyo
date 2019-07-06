#lang racket/base

(require rackunit

         app-name-here/components/hash)

(provide
 hash-tests)

(define hash-tests
  (test-suite
   "hash"

   (test-case "passwords can be hashed and then verified"
     (define h (make-password-hash "supersecret"))
     (check-false (hash-matches? h "invalid"))
     (check-true (hash-matches? h "supersecret")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests hash-tests))
