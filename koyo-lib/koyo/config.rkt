#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/contract/base
         racket/string)

(provide
 (contract-out
  [current-option-name-prefix
   (parameter/c non-empty-string?)])
 define-option)

(define current-option-name-prefix
  (make-parameter "KOYO"))

(define (make-option-name s)
  (string-append (current-option-name-prefix) "_"
                 (string-replace (string-upcase (symbol->string s)) "-" "_")))

(define-syntax (define-option stx)
  (syntax-parse stx
    [(_ name:id
        e:expr ...)
     #'(define-option name #:default #f e ...)]

    [(_ name:id
        #:default default:expr
        e:expr ...)
     (with-syntax ([(body ...) (if (null? (syntax-e #'(e ...)))
                                   #'(name)
                                   #'(e ...))])
       #'(begin
           (define name
             (let ([name (or (getenv (make-option-name 'name)) default)])
               body ...))

           (provide name)))]))
