#lang racket/base

(provide
 lookup
 register!)

(define REGISTRY (make-hash))

(define (register! qualified-id job)
  (when (hash-has-key? REGISTRY qualified-id)
    (raise-user-error 'define-job "a job named ~s already exists" qualified-id))
  (hash-set! REGISTRY qualified-id job))

(define (lookup qualified-id)
  (hash-ref REGISTRY qualified-id))
