#lang racket/base

(require koyo/config
         rackunit)

(provide config-tests)

(current-option-name-prefix "KOYO_TESTS")
(putenv "KOYO_TESTS_D" "set from env")

(define-option a)

(define-option b
  #:default "not set")

(define-option c
  #:default 42
  (add1 c))

(define-option d)

(define config-tests
  (test-suite
   "config"

   (test-suite
    "define-option"

    (test-case "options default to #f"
      (check-false a))

    (test-case "options can have custom defaults"
      (check-equal? b "not set"))

    (test-case "options can modify their values"
      (check-equal? c 43))

    (test-case "options can retrieve values from the environment"
      (check-equal? d "set from env")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests config-tests))
