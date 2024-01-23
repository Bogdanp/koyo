#lang racket/base

(require racket/contract/base
         racket/fixnum
         racket/port
         racket/random
         racket/string)

(provide
 (contract-out
  [current-random-string-generator (parameter/c (-> exact-positive-integer? non-empty-string?))]
  [generate-random-string (->* [] [exact-positive-integer?] non-empty-string?)]))

(define (crypto-generate-random-string len)
  (define num-bs
    (quotient
     (if (odd? len)
         (add1 len)
         len)
     2))
  (define str
    (with-output-to-string
      (lambda ()
        (for ([b (in-bytes (crypto-random-bytes num-bs))])
          (when (fx< b 16)
            (display "0"))
          (display (number->string b 16))))))
  (if (odd? len)
      (substring str 0 len)
      str))

(define current-random-string-generator
  (make-parameter crypto-generate-random-string))

(define (generate-random-string [len 64])
  ((current-random-string-generator) len))
