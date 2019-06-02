#lang racket/base

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(reprovide
 "../pages/auth.rkt"
 "../pages/common.rkt"
 "../pages/dashboard.rkt")
