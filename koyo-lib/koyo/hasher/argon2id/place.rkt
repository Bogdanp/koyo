#lang racket/base

(require racket/match
         racket/place
         "foreign.rkt")

(provide
 hasher-start)

(define-logger argon2id-hasher)

(define (hasher-start)
  (place ch
    (file-stream-buffer-mode (current-error-port) 'none)
    (define config (place-channel-get ch))
    (log-argon2id-hasher-debug "received config: ~s" config)
    (log-argon2id-hasher-debug "ready for messages")
    (let loop ()
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (log-argon2id-hasher-error "unhandled error: ~a" (exn-message e))
                         (loop))])

        (define message (place-channel-get ch))
        (log-argon2id-hasher-debug "received message: ~.s" message)
        (match message
          [`(stop)
           (log-argon2id-hasher-debug "stopped")]

          [`(hash-string ,str ,out)
           (with-handlers ([exn:fail? (λ (e) (place-channel-put out `(err ,(exn-message e))))])
             (place-channel-put out `(ok ,(hash-string str config))))
           (loop)]

          [`(hash-verify ,str ,h ,out)
           (with-handlers ([exn:fail? (λ (e) (place-channel-put out `(err ,(exn-message e))))])
             (place-channel-put out `(ok ,(hash-verify str h))))
           (loop)])))))
