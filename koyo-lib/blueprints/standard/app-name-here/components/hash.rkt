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

(module+ test
  (require rackunit
           rackunit/text-ui)

  (run-tests
   (test-suite
    "hash"

    (test-case "passwords can be hashed and then verified"
      (define h (make-password-hash "supersecret"))
      (check-false (hash-matches? h "invalid"))
      (check-true (hash-matches? h "supersecret"))))))
