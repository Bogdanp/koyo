#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [id-string?
   (-> string? boolean?)]
  [integer-string?
   (-> string? boolean?)]
  [real-string?
   (-> string? boolean?)]
  [string->id
   (-> string? (or/c #f exact-integer?))]
  [string->integer
   (-> string? (or/c #f exact-integer?))]
  [string->real
   (-> string? (or/c #f exact-integer?))]))

;; string->number should not be used to parse untrusted data. A string
;; like #e1e99999 will cause the process to hang and OOM.

(define (id-string? s)
  (regexp-match? #px"^(0|[1-9][0-9]{0,12})$" s))

(define (integer-string? s)
  (regexp-match? #px"^-?(0|[1-9][0-9]{0,12})$" s))

(define (real-string? s)
  (regexp-match? #px"^-?(0|[1-9][0-9]{0,12})(\\.[0-9]{0,12})?$" s))

(define (string->id s)
  (and (id-string? s)
       (string->number s)))

(define (string->integer s)
  (and (integer-string? s)
       (string->number s)))

(define (string->real s)
  (and (real-string? s)
       (string->number s)))
