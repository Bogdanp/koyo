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
     (check-equal? bs null))

   (test-case "define-guard"
     (define-guard (guard-never)
       #f)
     (check-false
      (with-guard (λ () #f)
        (guard-never)
        #t))

     (define-guard (guard-never+else)
       #:else 42
       #f)
     (check-equal?
      (with-guard (λ () #f)
        (guard-never+else)
        #t)
      42)

     (define-guard (guard-string? s)
       (string? s))
     (check-false
      (with-guard (λ () #f)
        (guard-string? #t)))
     (check-true
      (with-guard (λ () #f)
        (guard-string? "hello")
        #t)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests guard-tests))
