#lang racket/base

(require koyo/profiler
         rackunit)

(provide
 profiler-tests)

(define profiler-tests
  (test-suite
   "profiler"

   (test-suite
    "record-timing"

    (parameterize [(current-profile (make-profile))]
      (record-timing #:description "GET /"
                     (lambda ()
                       (record-timing #:label 'db
                                      #:description "SELECT * FROM users WHERE id = 1"
                                      (lambda ()
                                        (record-timing #:description "SELECT * FROM profiles WHERE user_id = 1" void)
                                        (record-timing #:description "SELECT * FROM profiles WHERE user_id = 1" void)))))

      (define timings (profile-timings (current-profile)))
      (check-equal? (length timings) 4)
      (check-equal? (map timing-id timings) '(1 2 4 3))
      (check-equal? (map timing-parent timings) '(0 1 2 2))
      (check-equal? (map timing-label timings) '(root db db db))))))


(module+ test
  (require rackunit/text-ui)
  (run-tests profiler-tests))
