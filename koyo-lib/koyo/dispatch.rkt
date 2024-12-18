#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/function
         web-server/dispatch
         web-server/dispatchers/dispatch)

(provide
 dispatch-rules+roles)

(define (default-else-proc _req)
  (next-dispatcher))

(begin-for-syntax
  (define-syntax-class dispatch-proc
    (pattern proc:id
             #:with name #''proc)
    (pattern (proc:dispatch-proc e ...)
             #:with name #'proc.name)))

(define-syntax (dispatch-rules+roles stx)
  (syntax-parse stx
    #:literals (else)
    [(_
      [pat
       {~alt {~optional {~seq #:method method}}
             {~optional {~seq #:roles (role:id ...)}}
             {~optional {~seq #:name name}}} ...
       proc:dispatch-proc] ...
      [else else-proc:expr])
     (with-syntax ([(proc-reverse-name ...)
                    (map (lambda (given-name proc-name)
                           (if (syntax->datum given-name)
                               given-name
                               proc-name))
                         (syntax->list #'({~? name #f} ...))
                         (syntax->list #'(proc.name ...)))])
       (syntax/loc stx
         (let-values ([(dispatch _)
                       (dispatch-rules
                        [pat {~? {~@ #:method method}} proc] ...
                        [else else-proc])]
                      [(reverse-uri)
                       (dispatch-url
                        [pat proc-reverse-name] ...)]
                      [(roles)
                       (dispatch-case
                        [pat {~? {~@ #:method method}} (const {~? '(role ...) null})] ...
                        [else (const null)])])
           (values dispatch reverse-uri roles))))]

    [(_
      [pat
       {~alt {~optional {~seq #:method method}}
             {~optional {~seq #:roles (role:id ...)}}
             {~optional {~seq #:name name}}} ...
       proc] ...)
     (syntax/loc stx
       (dispatch-rules+roles
        [pat {~? {~@ #:method method}}
             {~? {~@ #:roles (role ...)}}
             {~? {~@ #:name name}}
             proc] ...
        [else default-else-proc]))]))
