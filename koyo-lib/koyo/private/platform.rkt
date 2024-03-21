#lang racket/base

(require (for-syntax racket/base
                     setup/cross-system)
         racket/runtime-path
         setup/cross-system)

(provide
 maximize-fd-limit!)

(define-runtime-module-path-index module.rkt
  #:runtime?-id runtime?
  (case (if runtime?
            (system-type)
            (cross-system-type))
    [(macosx) '(lib "koyo/private/platform/macos.rkt")]
    [else '(lib "koyo/private/platform/stub.rkt")]))

(define-values (maximize-fd-limit!)
  ((dynamic-require module.rkt 'platform-values)))
