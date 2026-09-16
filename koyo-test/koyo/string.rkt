#lang racket/base

(require koyo/string
         rackcheck
         rackunit)

(provide
 string-tests)

(define gen:uint64
  (gen:integer-in 0 #xFFFFFFFFFFFFFFFF))
(define gen:int64
  (gen:integer-in #x-8000000000000000 #xFFFFFFFFFFFFFFFF))

(define string-tests
  (test-suite
   "string"

   (test-case "string->id"
     (check-false (string->id ""))
     (check-false (string->id "-1"))
     (check-false (string->id "1.5"))
     (check-property
      (property ([n gen:uint64])
        (equal? n (string->id (number->string n))))))

   (test-case "string->integer"
     (check-false (string->integer ""))
     (check-false (string->integer "1.5"))
     (check-property
      (property ([n gen:int64])
        (equal? n (string->integer (number->string n))))))

   (test-case "string->real"
     (check-false (string->real ""))
     (check-false (string->real "#e1e99999"))
     (check-equal? (string->real "1e10") 1e10)
     (check-equal? (string->real "1e-10") 1e-10)
     (check-property
      (property ([m gen:real]
                 [k gen:int64]
                 [n (gen:const (* m k))])
        (check-equal? n (string->real (number->string n))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests string-tests))
