#lang racket/base

(require koyo/guard
         rackunit)

(provide
 guard-tests)

(define guard-tests
  (test-suite
   "guard"

   (check-equal?
    (with-guard (λ () 'not-found)
      (guard #f)
      (error 'not-reachable))
    'not-found)

   (check-equal?
    (with-guard (λ () 'not-found)
      (guard #t)
      'found)
    'found)

   (check-equal?
    (with-guard (λ () 'not-found)
      (guard #f #:else 'bad-request)
      (error 'not-reachable))
    'bad-request)

   (let-values ([(as bs)
                 (with-guard (λ () (values null null))
                   (guard #f)
                   (error 'not-reachable))])
     (check-equal? as null)
     (check-equal? bs null))

   (let-values ([(as bs)
                 (with-guard (λ () (error "fail"))
                   (guard #f #:else (values null null))
                   (error 'not-reachable))])
     (check-equal? as null)
     (check-equal? bs null))))

(module+ test
  (require rackunit/text-ui)
  (run-tests guard-tests))
