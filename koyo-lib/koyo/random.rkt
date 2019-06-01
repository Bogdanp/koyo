#lang racket/base

(require racket/contract
         racket/format
         racket/random
         racket/string)

(provide
 current-random-string-generator
 generate-random-string)

(define (crypto-generate-random-string len)
  (string-join
   (for/list ([byte (in-bytes (crypto-random-bytes (/ len 2)))])
     (~a (number->string byte 16)
         #:align 'right
         #:min-width 2
         #:pad-string "0"))
   ""))

(define/contract current-random-string-generator
  (parameter/c (-> exact-positive-integer? non-empty-string?))
  (make-parameter crypto-generate-random-string))

(define/contract (generate-random-string [len 64])
  (->* () (exact-positive-integer?) non-empty-string?)
  ((current-random-string-generator) len))
