#lang racket/base

(module+ main
  (require rackunit
           rackunit/text-ui
           "components/all-component-tests.rkt")

  (run-tests
   (test-suite
    "app-name-here"

    component-tests)))
