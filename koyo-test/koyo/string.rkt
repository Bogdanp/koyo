#lang racket/base

(require koyo/string
         rackcheck
         rackunit)

(provide
 string-tests)

(define string-tests
  (test-suite
   "string"

   (test-case "string->id"
     (check-false (string->id ""))
     (check-false (string->id "-1"))
     (check-false (string->id "1.5"))
     (check-property
      (property ([n (gen:integer-in 0 #xFFFFFFFFFF)])
        (equal? n (string->id (number->string n))))))

   (test-case "string->integer"
     (check-false (string->integer ""))
     (check-false (string->integer "1.5"))
     (check-property
      (property ([n (gen:integer-in #x-FFFFFFFFFF #xFFFFFFFFFF)])
        (equal? n (string->integer (number->string n))))))

   (test-case "string->real"
     (check-false (string->real ""))
     (check-false (string->real "1e10"))
     (check-false (string->real "#e1e99999"))
     (check-property
      (property ([n gen:real]
                 [m (gen:integer-in #x-FFFFFFFFFF #xFFFFFFFFFF)])
        (let ([n (* n m)])
          (check-equal? n (string->real (number->string n)))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests string-tests))
