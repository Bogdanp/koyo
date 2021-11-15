#lang racket/base

(require racket/contract
         racket/fixnum
         racket/port
         racket/random
         racket/string)

(provide
 current-random-string-generator
 generate-random-string)

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

(define/contract current-random-string-generator
  (parameter/c (-> exact-positive-integer? non-empty-string?))
  (make-parameter crypto-generate-random-string))

(define/contract (generate-random-string [len 64])
  (->* () (exact-positive-integer?) non-empty-string?)
  ((current-random-string-generator) len))
