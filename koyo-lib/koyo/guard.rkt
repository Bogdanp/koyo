#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/stxparam)

(provide
 define-guard
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

(define-syntax (define-guard stx)
  (syntax-parse stx
    [(_ (guard-id:id arg-id:id ...)
        {~optional {~seq #:else else-expr:expr}}
        guard-expr:expr)
     #'(define-syntax (guard-id stx)
         (syntax-parse stx
           [(_ arg:expr (... ...))
            #'(guard
               ((lambda (arg-id ...)
                  guard-expr)
                arg (... ...))
               {~? {~@ #:else else-expr}})]))]))
