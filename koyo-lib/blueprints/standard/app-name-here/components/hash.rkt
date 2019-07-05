#lang racket/base

(require racket/contract
         racket/place
         racket/string
         "hasher.rkt")

(provide
 make-password-hash
 hash-matches?)

(define hasher-place-mu
  (make-semaphore 1))

(define hasher-place-ch
  (hasher-start))

(define/contract (make-password-hash s)
  (-> string? string?)
  (call-with-semaphore hasher-place-mu
    (lambda _
      (place-channel-put/get hasher-place-ch (list 'hash s)))))

(define/contract (hash-matches? h s)
  (-> string? string? boolean?)
  (call-with-semaphore hasher-place-mu
    (lambda _
      (place-channel-put/get hasher-place-ch (list 'verify s h)))))
