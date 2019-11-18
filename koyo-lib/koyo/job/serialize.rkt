#lang racket/base

(require racket/fasl
         (prefix-in s: racket/serialize))

(provide
 deserialize
 serialize)

(define deserialize
  (compose1 s:deserialize fasl->s-exp))

(define serialize
  (compose1 s-exp->fasl s:serialize))
