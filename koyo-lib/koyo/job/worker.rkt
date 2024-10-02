#lang racket/base

(require component
         racket/contract/base
         racket/match
         racket/string
         "broker.rkt"
         "reactor.rkt")

;; worker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 worker?
 (contract-out
  [make-worker-factory
   (->* []
        [#:queue non-empty-string?
         #:pool-size exact-positive-integer?]
        (-> broker? worker?))]))

(struct worker (broker queue pool-size reactor)
  #:transparent
  #:methods gen:component
  [(define (component-start w)
     (match-define (worker broker queue pool-size _) w)
     (struct-copy worker w [reactor (reactor broker queue pool-size)]))

   (define (component-stop w)
     (stop-reactor (worker-reactor w))
     (struct-copy worker w [reactor #f]))])

(define ((make-worker-factory
          #:queue [queue "default"]
          #:pool-size [pool-size 8]) broker)
  (worker broker queue pool-size #f))
