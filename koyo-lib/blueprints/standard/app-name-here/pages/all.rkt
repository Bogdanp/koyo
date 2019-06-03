#lang racket/base

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(reprovide
 "auth.rkt"
 "common.rkt"
 "dashboard.rkt")
