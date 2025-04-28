#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/stxparam)

(provide
 with-guard
 guard)

(define-syntax-parameter guard
  (lambda (stx)
    (raise-syntax-error 'guard "guard used outside of with-guard form" stx)))

(define-syntax (with-guard stx)
  (syntax-parse stx
    [(_ else-proc body ...+)
     #'(let ([else-proc-tmp else-proc])
         (let/ec return
           (syntax-parameterize ([guard
                                  (lambda (stx)
                                    (syntax-parse stx
                                      [(_ v:expr)
                                       #'(or v (call-with-values else-proc-tmp return))]
                                      [(_ v:expr #:else result-expr:expr)
                                       #'(or v (call-with-values (λ () result-expr) return))]))])
             body ...)))]))
