#lang racket/base

(require review/ext
         syntax/parse/pre)

#|review: ignore|#

(provide
 should-review-syntax?
 review-syntax)

(define (should-review-syntax? stx)
  (syntax-case stx (define-option)
    [(define-option . _rest) #t]
    [_ #f]))

(define-syntax-class option-definition
  #:datum-literals (define-option)
  (pattern (define-option id:id
             {~optional {~seq #:default _default-expr:expression}}
             {~do (push-scope)}
             _body:expression ...
             {~do (pop-scope)})
           #:do [(track-binding #'id "~a" #:check-usages? #f)]))

(define (review-syntax stx)
  (syntax-parse stx
    [c:option-definition #'c]
    [_ (track-error stx "expected a option definition")]))
