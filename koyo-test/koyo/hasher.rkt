#lang racket/base

(require component
         koyo/hasher
         rackunit)

(provide
 hasher-tests)

(define hasher-tests
  (let ([h ((make-argon2id-hasher-factory
             #:parallelism 1
             #:iterations 128
             #:memory 1024))])
    (test-suite
     "hasher"

     #:before
     (lambda ()
       (set! h (component-start h)))

     #:after
     (lambda ()
       (set! h (component-stop h)))

     (test-case "passwords can be hashed and then verified"
       (define pass-hash (hasher-make-hash h "supersecret"))
       (check-false (hasher-hash-matches? h pass-hash "invalid"))
       (check-true (hasher-hash-matches? h pass-hash "supersecret"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests hasher-tests))
