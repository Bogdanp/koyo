#lang racket/base

(require (prefix-in koyo: koyo/console)
         racket/path)

(provide
 start-console)

(define here
  (path-only (syntax-source #'here)))
(define dynamic.rkt
  (build-path here "dynamic.rkt"))

(define (start-console)
  (define namespace (make-base-empty-namespace))
  (current-namespace namespace)
  (koyo:stubbed-components `(server))
  (koyo:start-console dynamic.rkt namespace))

(module+ main
  (start-console))
