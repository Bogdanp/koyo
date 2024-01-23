#lang racket/base

(require component
         racket/contract/base
         racket/match
         racket/place
         "argon2id-place.rkt"
         "error.rkt"
         "generic.rkt")

(provide
 argon2id-hasher?
 (contract-out
  [make-argon2id-hasher-factory
   (->* []
        [#:parallelism exact-positive-integer?
         #:iterations exact-positive-integer?
         #:memory exact-positive-integer?]
        (-> argon2id-hasher?))]))

(struct argon2id-hasher (config sema [ch #:mutable] running?)
  #:methods gen:component
  [(define (component-start h)
     (struct-copy argon2id-hasher h [running? #t]))

   (define (component-stop h)
     (try-stop-hasher-place! h)
     (struct-copy argon2id-hasher h [running? #f]))]

  #:methods gen:hasher
  [(define (hasher-make-hash h pass)
     (try-start-hasher-place! 'hasher-make-hash h)
     (define pch (argon2id-hasher-ch h))
     (define-values (in out)
       (place-channel))
     (place-channel-put pch `(hash ,pass ,out))
     (sync
      (command-result-evt 'hasher-make-hash in)
      (handle-evt
       (place-dead-evt pch)
       (λ (_) (oops 'hasher-make-hash "place crashed")))))

   (define (hasher-hash-matches? h pass-hash pass)
     (try-start-hasher-place! 'hasher-hash-matches? h)
     (define pch (argon2id-hasher-ch h))
     (define-values (in out)
       (place-channel))
     (place-channel-put pch `(verify ,pass ,pass-hash ,out))
     (sync
      (command-result-evt 'hasher-hash-matches? in)
      (handle-evt
       (place-dead-evt pch)
       (λ (_) (oops 'hasher-hash-matches? "place crashed")))))])

(define ((make-argon2id-hasher-factory
          #:parallelism [parallelism (processor-count)]
          #:iterations [iterations 256]
          #:memory [memory 2048]))
  (define config
    `((p ,parallelism)
      (t ,iterations)
      (m ,memory)))
  (argon2id-hasher config (make-semaphore 1) #f #f))

(define (try-start-hasher-place! who h)
  (unless (argon2id-hasher-running? h)
    (oops who "hasher component is not running"))
  (call-with-semaphore (argon2id-hasher-sema h)
    (lambda ()
      (unless (argon2id-hasher-ch h)
        (define ch (hasher-start))
        (place-channel-put ch (argon2id-hasher-config h))
        (set-argon2id-hasher-ch! h ch)))))

(define (try-stop-hasher-place! h)
  (call-with-semaphore (argon2id-hasher-sema h)
    (lambda ()
      (define ch (argon2id-hasher-ch h))
      (when ch
        (place-channel-put ch '(stop))
        (set-argon2id-hasher-ch! h #f)))))

(define (command-result-evt who in)
  (handle-evt
   in
   (match-lambda
     [`(err ,msg) (oops who "~a" msg)]
     [`(ok ,res) res])))
