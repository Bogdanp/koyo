#lang racket/base

(define-syntax-rule (require/provide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(require/provide
 "../pages/auth.rkt"
 "../pages/common.rkt"
 "../pages/dashboard.rkt")
