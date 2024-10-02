#lang racket/base

(require (for-syntax racket/base
                     racket/stxparam
                     syntax/parse/pre)
         gregor
         racket/stxparam
         "broker.rkt"
         "registry.rkt")


;; retries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 exn:job:retry?
 exn:job:retry-delay-ms
 retry!)

(struct exn:job exn () #:transparent)
(struct exn:job:retry exn:job (delay-ms) #:transparent)

(define-syntax-parameter retry!
  (lambda (stx)
    (raise-syntax-error #f "retry may only be used inside define-job" stx)))


;; jobs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 execute-jobs-synchronously?
 define-job
 job?
 job-proc)

(define execute-jobs-synchronously?
  (make-parameter #f))

(define current-job-scheduled-at
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
          (raise-user-error
           #;who-sym (job-id job)
           #;format-str "(current-broker) is not set"))
        (broker-enqueue!
         broker
         (job-queue job)
         (job-id job)
         (job-priority job)
         (or (current-job-scheduled-at)
             (now/moment))
         (list kws kw-args args))]))))

(define (make-job #:id id
                  #:queue queue
                  #:priority priority
                  #:proc proc)
  (define the-job (job id queue priority proc))
  (begin0 the-job
    (register! (format "~a.~a" queue id) the-job)))

(define-syntax (do-retry stx)
  (syntax-parse stx
    [(_ duration-ms:expr)
     #'(retry! "retry!" duration-ms)]

    [(_ reason:string duration-ms:expr)
     #'(raise (exn:job:retry reason (current-continuation-marks) duration-ms))]))

(define-syntax (define-job stx)
  (syntax-parse stx
    [(_ (~or (id:id arg ...)
             (id:id arg ... . rest-id:id))
        (~alt (~optional (~seq #:queue queue:str) #:name "#:queue parameter")
              (~optional (~seq #:priority priority:number) #:name "#:priority parameter")) ...
        e:expr ...+)
     #'(define id
         (syntax-parameterize ([retry! (make-rename-transformer #'do-retry)])
           (make-job
            #:id 'id
            #:queue (~? queue "default")
            #:priority (~? priority 50)
            #:proc (procedure-rename
                    (lambda (~? (arg ... . rest-id) (arg ...))
                      e ...)
                    'id))))]))


;; scheduling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 schedule-at)

(define-syntax (schedule-at stx)
  (syntax-parse stx
    [(_ when-e:expr job-e:expr)
     #'(parameterize ([current-job-scheduled-at when-e])
         job-e)]))
