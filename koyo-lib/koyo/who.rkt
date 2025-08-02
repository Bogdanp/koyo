#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/stxparam)

(provide
 who
 define/who)

(define-syntax-parameter who
  (lambda (stx)
    (raise-syntax-error #f "who may not be used outside of a define/syntax form" stx)))

(define-syntax (define/who stx)
  (syntax-parse stx
    [(_ (id . args) body ...+)
     #'(define (id . args)
         (syntax-parameterize ([who (lambda (_stx) #''id)])
           body ...))]))
