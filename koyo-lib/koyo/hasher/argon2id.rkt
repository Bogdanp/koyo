#lang racket/base

(require (for-syntax racket/base)
         component
         racket/contract/base
         racket/match
         racket/place
         version-case
         "../who.rkt"
         "argon2id/foreign.rkt"
         "argon2id/place.rkt"
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

(struct argon2id-hasher (config mode sema-or-pool [ch #:mutable] running?)
  #:methods gen:component
  [(define (component-start h)
     (struct-copy argon2id-hasher h [running? #t]))

   (define (component-stop h)
     (when (eq? (argon2id-hasher-mode h) 'place)
       (try-stop-hasher-place! h))
     (struct-copy argon2id-hasher h [running? #f]))]

  #:methods gen:hasher
  [(define/who (hasher-make-hash h pass)
     (case (argon2id-hasher-mode h)
       [(place) (run-in-place who h 'hash-string pass)]
       [(thread) (run-in-thread who h (λ () (hash-string pass (argon2id-hasher-config h))))]
       [else (oops who "internal error: invalid mode")]))

   (define/who (hasher-hash-matches? h pass-hash pass)
     (case (argon2id-hasher-mode h)
       [(place) (run-in-place who h 'hash-verify pass pass-hash)]
       [(thread) (run-in-thread who h (λ () (hash-verify pass pass-hash)))]
       [else (oops who "internal error: invalid mode")]))])

(define ((make-argon2id-hasher-factory
          #:parallelism [parallelism (processor-count)]
          #:iterations [iterations 256]
          #:memory [memory 2048]))
  (define config
    `((p ,parallelism)
      (t ,iterations)
      (m ,memory)))
  (define-values (mode sema-or-pool)
    (version-case
     [(version< (version) "8.18.0.9")
      (values 'place (make-semaphore 1))]
     [else
      (values 'thread (make-parallel-thread-pool 1))]))
  (argon2id-hasher config mode sema-or-pool #f #f))

(define (run-in-thread who h proc)
  (thread-wait
   (thread
    #:pool (argon2id-hasher-sema-or-pool h)
    #:keep 'results
    proc)
   (lambda ()
     (oops who "thread crashed"))))

(define (run-in-place who h . message)
  (try-start-hasher-place! who h)
  (define pch (argon2id-hasher-ch h))
  (define-values (in out)
    (place-channel))
  (place-channel-put pch (append message (list out)))
  (sync
   (choice-evt
    (handle-evt
     in
     (match-lambda
       [`(ok ,res) res]
       [`(err ,msg) (oops who "~a" msg)]))
    (handle-evt
     (place-dead-evt pch)
     (lambda (_)
       (oops who "place crashed"))))))

(define (try-start-hasher-place! who h)
  (unless (argon2id-hasher-running? h)
    (oops who "hasher component is not running"))
  (call-with-semaphore (argon2id-hasher-sema-or-pool h)
    (lambda ()
      (unless (argon2id-hasher-ch h)
        (define ch (hasher-start))
        (place-channel-put ch (argon2id-hasher-config h))
        (set-argon2id-hasher-ch! h ch)))))

(define (try-stop-hasher-place! h)
  (call-with-semaphore (argon2id-hasher-sema-or-pool h)
    (lambda ()
      (define ch (argon2id-hasher-ch h))
      (when ch
        (place-channel-put ch '(stop))
        (set-argon2id-hasher-ch! h #f)))))
