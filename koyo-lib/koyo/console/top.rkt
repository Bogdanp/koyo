#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         component)

(provide
 (rename-out [console-#%top #%top]))

(define-syntax (console-#%top stx)
  (syntax-parse stx
    [(_ . id:id)
     #:when (regexp-match? #rx"^[@]" (symbol->string (syntax-e #'id)))
     #:with component-id (string->symbol (substring (symbol->string (syntax-e #'id)) 1))
     #'(system-ref 'component-id)]
    [(_ . id:id)
     #'(#%top . id)]))
