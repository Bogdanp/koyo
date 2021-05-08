#lang racket/base

(require (for-label koyo
                    racket/base)
         (for-syntax racket/base)
         racket/runtime-path
         racket/sandbox
         scribble/example
         scribble/manual)

(provide (all-from-out scribble/example
                       scribble/manual)
         media-path
         sandbox)

(define-runtime-path media-path
  (build-path 'up "media"))

(define sandbox
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-memory-limit 256])
       (make-evaluator 'racket/base)))))

(void (examples #:eval sandbox (require component koyo)))


(provide
 haml-form
 haml-splicing-syntax-example)

(define haml-form
  (defform
    #:literals (unquote-splicing unless when)
    (haml element ...+)
    #:grammar
    [(element (selector maybe-attributes element ...)
              (unless cond-expr e0 e ...)
              (when cond-expr e0 e ...)
              &html-entity-name
              ,@expr
              expr)

     (selector :tag-name
               .class-name
               .class-name-1.class-name-2.class-name-n#id
               :tag-name.class-name
               :tag-name.class-name#id)

     (maybe-attributes (code:line)
                       ([:attribute-name maybe-expr] ...))]
    "Produces an x-expression."))

(define haml-splicing-syntax-example
  (examples #:eval sandbox
            #:label #f
            (haml
             (.content
              (:ul.items
               ,@(for/list ([it (list "a" "b" "c")])
                   (haml (:li it))))))))
