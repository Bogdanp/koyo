#lang racket/base

(require component
         racket/contract/base
         racket/match
         racket/string
         "broker.rkt"
         "job-metadata.rkt"
         "reactor.rkt")

;; worker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 worker?
 (contract-out
  [make-worker-factory
   (->* []
        [#:queue non-empty-string?
         #:pool-size exact-positive-integer?
         #:middleware (-> job-metadata? procedure? procedure?)]
        (-> broker? worker?))]))

(struct worker (broker queue pool-size middleware reactor)
  #:transparent
  #:methods gen:component
  [(define (component-start w)
     (match-define (worker broker queue pool-size middleware _) w)
     (define the-reactor (reactor broker queue pool-size middleware))
     (struct-copy worker w [reactor the-reactor]))

   (define (component-stop w)
     (stop-reactor (worker-reactor w))
     (struct-copy worker w [reactor #f]))])

(define ((make-worker-factory
          #:queue [queue "default"]
          #:pool-size [pool-size 8]
          #:middleware [middleware (λ (_meta proc) proc)]) broker)
  (worker broker queue pool-size middleware #f))
