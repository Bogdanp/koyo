#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/function
         web-server/dispatch
         web-server/dispatchers/dispatch)

(provide
 dispatch-rules+roles)

(begin-for-syntax
  (define-syntax-class dispatch-fun
    (pattern fun:id
             #:attr name #''fun)

    (pattern (fun:dispatch-fun e ...)
             #:attr name (attribute fun.name))))

(define-syntax (dispatch-rules+roles stx)
  (syntax-parse stx
    #:literals (else)
    [(_
      [pat
       (~alt (~optional (~seq #:method method)       #:defaults ([method #'"get"]))
             (~optional (~seq #:roles (role:id ...)) #:defaults ([(role 1) null]))
             (~optional (~seq #:name name)           #:defaults ([name #'#f]))) ...
       fun:dispatch-fun]
      ...
      [else else-fun])
     (with-syntax ([(fun-reverse-name ...)
                    (map (lambda (given-name default-name)
                           (if (syntax->datum given-name)
                               given-name
                               default-name))
                         (syntax->list #'(name ...))
                         (syntax->list #'(fun.name ...)))])
       (syntax/loc stx
         (let-values ([(dispatch _)
                       (dispatch-rules
                        [pat #:method method fun] ...
                        [else else-fun])]
                      [(reverse-uri)
                       (dispatch-url
                        [pat fun-reverse-name] ...)]
                      [(roles)
                       (dispatch-case
                        [pat #:method method (const '(role ...))] ...
                        [else (const null)])])
           (values dispatch reverse-uri roles))))]

    [(_
      [pat
       (~alt (~optional (~seq #:method method)       #:defaults ([method #'"get"]))
             (~optional (~seq #:roles (role:id ...)) #:defaults ([(role 1) null]))
             (~optional (~seq #:name name)           #:defaults ([name #'#f]))) ...
       fun] ...)
     (syntax/loc stx
       (dispatch-rules+roles
        [pat #:method method #:roles (role ...) #:name name fun] ...
        [else next-dispatcher]))]))
