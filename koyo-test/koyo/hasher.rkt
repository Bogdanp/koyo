#lang racket/base

(require component/testing
         koyo/hasher
         rackunit)

(provide
 hasher-tests)

(define hasher-tests
  (system-test-suite hasher-tests ([a (h) void]
                                   [h (make-argon2id-hasher-factory
                                       #:parallelism 1
                                       #:iterations 128
                                       #:memory 1024)])
    (test-case "passwords can be hashed and then verified"
      (define pass-hash (hasher-make-hash h "supersecret"))
      (check-false (hasher-hash-matches? h pass-hash "invalid"))
      (check-true (hasher-hash-matches? h pass-hash "supersecret")))))

(module+ test
  (require rackunit/text-ui)
  (unless (getenv "PLT_PKG_BUILD_SERVICE")
    (run-tests hasher-tests)))
