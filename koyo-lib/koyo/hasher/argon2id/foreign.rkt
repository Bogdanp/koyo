#lang racket/base

(require crypto
         crypto/argon2)

(provide
 hash-string
 hash-verify)

;; These operations block the Racket VM, so they should only be called
;; in either a separate Place, as in place.rkt, or in a parallel
;; thread (in 8.18.0.9+).

(define (hash-string str config)
  (parameterize ([crypto-factories (list argon2-factory)])
    (pwhash 'argon2id (string->bytes/utf-8 str) config)))

(define (hash-verify str h)
  (parameterize ([crypto-factories (list argon2-factory)])
    (pwhash-verify #f (string->bytes/utf-8 str) h)))
