#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         "broker.rkt"
         "registry.rkt")

(provide
 execute-jobs-synchronously?
 define-job
 job?
 job-proc)

(define/contract execute-jobs-synchronously?
  (parameter/c boolean?)
  (make-parameter #f))

(struct job (id queue priority proc)
  #:transparent
  #:property
  prop:procedure
  (make-keyword-procedure
   (lambda (kws kw-args job . args)
     (cond
       [(execute-jobs-synchronously?)
        (begin0 #f
          (keyword-apply (job-proc job) kws kw-args args))]

       [else
        (define broker (current-broker))
        (unless broker
          (raise-user-error (job-id job) "(current-broker) is not set"))
        (broker-enqueue! broker
                         (job-queue job)
                         (job-id job)
                         (job-priority job)
                         (list kws kw-args args))]))))

(define (make-job #:id id
                  #:queue queue
                  #:priority priority
                  #:proc proc)
  (define the-job (job id queue priority proc))
  (begin0 the-job
    (register! (format "~a.~a" queue id) the-job)))

(define-syntax (define-job stx)
  (syntax-parse stx
    [(_ (~or (id:id arg ...)
             (id:id arg ... . rest-id:id))
        (~alt (~optional (~seq #:queue queue:str) #:name "#:queue parameter")
              (~optional (~seq #:priority priority:number) #:name "#:priority parameter")) ...
        e:expr ...+)
     #'(define id
         (make-job #:id 'id
                   #:queue (~? queue "default")
                   #:priority (~? priority 50)
                   #:proc (procedure-rename
                           (lambda (~? (arg ... . rest-id) (arg ...))
                             e ...)
                           'id)))]))
