#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         component
         crontab)

(provide
 crontab*)

(struct crontab-component (factory-proc stop-proc)
  #:methods gen:component
  [(define (component-start c)
     (define stop ((crontab-component-factory-proc c)))
     (struct-copy crontab-component c [stop-proc stop]))
   (define (component-stop c)
     ((crontab-component-stop-proc c))
     (struct-copy crontab-component c [stop-proc void]))])

(define-syntax (crontab* stx)
  (syntax-parse stx
    [(_ [schedule:expr handler:expr] ...+)
     #'(crontab-component (λ () (crontab [schedule handler] ...)) void)]))
