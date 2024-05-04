#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         "haml.rkt")

(provide
 define-haml-template
 slot)

(define-syntax (slot stx)
  (raise-syntax-error 'slot "may only be used within a define-haml-template form" stx))

(begin-for-syntax
  (define (fill-slots stx content-stx)
    (syntax-parse stx
      [(rator . rands)
       #:with filled-rator (fill-slots #'rator content-stx)
       #:with filled-rands
       (apply
        append
        (for/list ([rand-stx (in-list (syntax-e #'rands))])
          (syntax-parse rand-stx
            #:literals (slot)
            [(slot) (syntax-e content-stx)]
            [_ (list (fill-slots rand-stx content-stx))])))
       #'(filled-rator . filled-rands)]
      [_ stx])))

(define-syntax (define-haml-template stx)
  (syntax-parse stx
    [(_ id:id template)
     #'(define-syntax (id stx)
         (syntax-parse stx
           #:literals (slot)
           [(_ body (... ...))
            #:with content (fill-slots #'template #'(body (... ...)))
            #'(haml content)]))]))
