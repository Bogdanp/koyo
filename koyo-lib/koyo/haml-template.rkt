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
  (define (find-named-slot kwd+contents kwd)
    (for/first ([pair-stx (in-list (syntax-e kwd+contents))])
      (syntax-parse pair-stx
        [(k:keyword c)
         #:when (equal? (syntax->datum #'k) kwd)
         #'c]
        [_ #f])))

  (define (fill-slots call-stx template-stx kwd+contents content-stx)
    (syntax-parse template-stx
      [(rator . rands)
       #:with filled-rator (fill-slots call-stx #'rator kwd+contents content-stx)
       #:with filled-rands
       (apply
        append
        (for/list ([rand-stx (in-list (syntax-e #'rands))])
          (syntax-parse rand-stx
            #:literals (slot)
            [(slot k:keyword {~optional d})
             (define kwd (syntax-e #'k))
             (define content-stx ;; noqa
               (or (find-named-slot kwd+contents kwd)
                   (and (attribute d) #'d)))
             (unless content-stx
               (define message (format "required keyword argument not supplied~n  required keyword: ~a" kwd))
               (raise-syntax-error #f message call-stx))
             (list (syntax-e content-stx))]
            [(slot) (syntax-e content-stx)]
            [_ (list (fill-slots call-stx rand-stx kwd+contents content-stx))])))
       #'(filled-rator . filled-rands)]
      [_ template-stx])))

(define-syntax (define-haml-template stx)
  (syntax-parse stx
    [(_ id:id template)
     #'(define-syntax (id stx)
         (syntax-parse stx
           #:literals (slot)
           [(_ {~seq k:keyword c} (... ...) . body)
            #:with kwds #'([k c] (... ...))
            #:with content (fill-slots stx #'template #'kwds #'body)
            #'(haml content)]))]))
