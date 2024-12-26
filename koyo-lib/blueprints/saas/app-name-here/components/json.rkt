#lang racket/base

(require db
         deta
         gregor
         json
         racket/generic)

(provide
 gen:to-jsexpr
 to-jsexpr?
 ->jsexpr)

(define-generics to-jsexpr
  {->jsexpr to-jsexpr}
  #:fast-defaults
  ([(lambda (v)
      (or
       (string? v)
       (number? v)
       (boolean? v)))
    (define ->jsexpr values)]
   [list?
    (define/generic super ->jsexpr)
    (define (->jsexpr l)
      (map super l))]
   [hash?
    (define/generic super ->jsexpr)
    (define (->jsexpr h)
      (for/hasheq ([(k v) (in-hash h)])
        (values k (super v))))]
   [symbol?
    (define ->jsexpr symbol->string)]
   [vector?
    (define/generic super ->jsexpr)
    (define (->jsexpr vs)
      (for/list ([v (in-vector vs)])
        (super v)))]
   [moment?
    (define ->jsexpr moment->iso8601)]
   [sql-null?
    (define (->jsexpr _)
      (json-null))])
  #:defaults
  ([entity?
    (define/generic super ->jsexpr)
    (define ->jsexpr (compose1 super entity->hash))]))
