#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in h: html)
         racket/contract
         racket/function
         racket/match
         racket/port
         racket/string
         xml)

(provide
 html->xexpr
 html->xexpr/first
 xexpr->text
 xexpr-select
 xexpr-select-first
 xexpr-select-text)

(define (default-surround s)
  (string-append "<div>" s "</div>"))

(define/contract (html->xexpr s [surround default-surround])
  (->* (string?)
       ((-> string? string?))
       (listof xexpr?))
  (call-with-input-string (surround s)
    (lambda (in)
      (map xml->xexpr (h:read-html-as-xml in)))))

(define html->xexpr/first
  (compose1 car html->xexpr))

(define/contract (xexpr->text e [sep " "])
  (->* (xexpr?) (string?) string?)
  (match e
    [(? symbol?) (format "&~a;" e)]
    [(? number?) (format "&#~a;" e)]

    [(and s (? string?)) s]

    [(list _ (list (list (? symbol?) (? string?)) ...) e ...)
     (string-join (map xexpr->text e) sep)]

    [(list _ e ...)
     (string-join (map xexpr->text e) sep)]))

(define selector/c
  (listof (or/c symbol?
                (listof (list/c symbol? string?)))))

(define (list-subset? xs ys)
  (andmap (curryr member xs) ys))

(define xexpr-select-matches?
  (match-lambda**
   [((list '* selector-attrs)
     (and (list _ element-attrs e ...) xexpr))
    (list-subset? element-attrs selector-attrs)]

   [((list tag selector-attrs)
     (and (list tag element-attrs e ...) xexpr))
    (list-subset? element-attrs selector-attrs)]

   [(_ _) #f]))

(define (xexpr-selector-select selector xexpr)
  (define matches?
    (curry xexpr-select-matches? selector))

  (let loop ([selected null]
             [remaining (list xexpr)])
    (cond
      [(null? remaining)
       (reverse selected)]

      [else
       (define-values (selected* remaining*)
         (for/fold ([selected selected]
                    [remaining null])
                   ([xexpr remaining])
           (match xexpr
             [(? matches?)
              (values (cons xexpr selected) remaining)]

             [(or (list _ (list (cons _ _) ...) es ...)
                  (list _ es ...))
              (values selected (append remaining es))]

             [_
              (values selected remaining)])))

       (loop selected* remaining*)])))

(define/contract (make-xexpr-selector selectors)
  (-> (listof selector/c)
      (-> xexpr? (listof xexpr?)))

  (define (xexpr-selector xexpr)
    (let loop ([selectors selectors]
               [selected (list xexpr)])
      (match selectors
        [(list) selected]
        [(cons selector selectors)
         (loop selectors
               (apply append (for/list ([xexpr selected])
                               (xexpr-selector-select selector xexpr))))])))

  xexpr-selector)

(begin-for-syntax
  (define-syntax-class selector
    (pattern tag:id
             #:with attrs #'())

    (pattern (tag:id [(attr:id value:str) ...+])
             #:with attrs #'((attr value) ...))))

(define-syntax (xexpr-select stx)
  (syntax-parse stx
    [(_ e:expr s:selector ...+)
     #'((make-xexpr-selector (list (list 's.tag 's.attrs) ...)) e)]))

(define-syntax (xexpr-select-first stx)
  (syntax-parse stx
    [(_ e:expr s ...+)
     #'(car (xexpr-select e s ...))]))

(define-syntax (xexpr-select-text stx)
  (syntax-parse stx
    [(_ e:expr s ...+)
     #'(map (curryr xexpr->text "") (xexpr-select e s ...))]))
