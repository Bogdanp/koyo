#lang racket/base

(require rackunit
         "config.rkt"
         "continuation.rkt"
         "cors.rkt"
         "csrf.rkt"
         "database.rkt"
         "dispatch.rkt"
         "flash.rkt"
         "haml.rkt"
         "l10n.rkt"
         "mime.rkt"
         "profiler.rkt"
         "session.rkt"
         "testing.rkt"
         "xexpr.rkt")

(define all-tests
  (test-suite
   "koyo"

   config-tests
   continuation-tests
   cors-tests
   csrf-tests
   database-tests
   dispatch-tests
   flash-tests
   haml-tests
   l10n-tests
   mime-tests
   profiler-tests
   session-tests
   testing-tests
   xexpr-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-tests))
