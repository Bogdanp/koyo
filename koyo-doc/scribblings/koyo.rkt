#lang racket/base

(require racket/sandbox
         scribble/example
         scribble/manual)
(provide (all-from-out scribble/example
                       scribble/manual)
         sandbox)

(define sandbox
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-memory-limit 50])
       (make-evaluator 'racket/base)))))

(void (examples #:eval sandbox (require koyo)))


(provide
 haml-form
 haml-at-syntax
 haml-at-syntax-example)

(define haml-form
  (defform
    (haml element)
    #:grammar
    [(element (selector attributes element ...)
              (selector element ...)
              symbol
              unless
              when
              splice
              expr)

     (selector :tag-name
               .class-name
               .class-name-1.class-name-2.class-name-n#id
               :tag-name.class-name
               :tag-name.class-name#id)

     (symbol &name)

     (attributes ((attribute-name maybe-expr) ...))
     (attribute-name :attribute-name)

     (unless (unless cond-expr e0 e ...))
     (when (when cond-expr e0 e ...))

     (splice (@ e))]
    "Produces an x-expression."))

(define haml-at-syntax
  (literal "(@ e)"))

(define haml-at-syntax-example
  (examples #:eval sandbox
            #:label #f
            (haml
             (.content
              (:ul.items
               (@ (for/list ([it (list "a" "b" "c")])
                    (haml (:li it)))))))))
