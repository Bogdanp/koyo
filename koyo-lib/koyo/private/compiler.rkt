#lang racket/base

(require compiler/cm)

(provide
 compile-zo)

(define (compile-zo path)
  (parameterize ([compile-enforce-module-constants #f]
                 [current-namespace (make-base-empty-namespace)]
                 [manager-compile-notify-handler
                  (lambda (mod-path)
                    (eprintf "  [compiling ~a]~n" mod-path))])
    (managed-compile-zo path)))
