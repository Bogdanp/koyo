#lang racket/base

(require review/ext
         syntax/parse/pre)

#|review: ignore|#

(provide
 should-review-syntax?
 review-syntax)

(define (should-review-syntax? stx)
  (syntax-case stx (define-option with-database-connection with-database-transaction)
    [(define-option . _rest) #t]
    [(with-database-connection . _rest) #t]
    [(with-database-transaction . _rest) #t]
    [_ #f]))

(define-syntax-class database-block
  #:datum-literals (with-database-connection with-database-transaction)
  (pattern (with-database-connection
             {~do (push-scope)}
             [conn-id:id db:expression]
             {~do (track-binding #'conn-id "~a")}
             body:expression ...+
             {~do (pop-scope)}))
  (pattern (with-database-transaction
             {~do (push-scope)}
             [conn-id:id db:expression]
             {~do (track-binding #'conn-id "~a")}
             {~optional {~seq #:isolation isolation:expression}}
             body:expression ...+
             {~do (pop-scope)})))

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
    [d:database-block #'d]
    [_ (track-error stx "expected a option definition")]))
