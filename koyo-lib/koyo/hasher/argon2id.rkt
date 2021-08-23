#lang racket/base

(require component
         racket/contract
         racket/place
         "argon2id-place.rkt"
         "generic.rkt")

(provide
 make-argon2id-hasher-factory
 argon2id-hasher?)

(define (fail-place-dead who)
  (error who "hasher place crashed"))

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
     (place-channel-put pch (list 'hash pass out))
     (sync
      in
      (handle-evt
       (place-dead-evt pch)
       (λ (_) (fail-place-dead 'hasher-make-hash)))))

   (define (hasher-hash-matches? h pass-hash pass)
     (try-start-hasher-place! 'hasher-hash-matches? h)
     (define pch (argon2id-hasher-ch h))
     (define-values (in out)
       (place-channel))
     (place-channel-put pch (list 'verify pass pass-hash out))
     (sync
      in
      (handle-evt
       (place-dead-evt pch)
       (λ (_) (fail-place-dead 'hasher-hash-matches?)))))])

(define/contract ((make-argon2id-hasher-factory
                   #:parallelism [parallelism (processor-count)]
                   #:iterations [iterations 256]
                   #:memory [memory 2048]))
  (->* ()
       (#:parallelism exact-positive-integer?
        #:iterations exact-positive-integer?
        #:memory exact-positive-integer?)
       (-> argon2id-hasher?))
  (define config `((p ,parallelism) (t ,iterations) (m ,memory)))
  (argon2id-hasher config (make-semaphore 1) #f #f))

(define (try-start-hasher-place! who h)
  (unless (argon2id-hasher-running? h)
    (error who "hasher component is stopped"))
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
