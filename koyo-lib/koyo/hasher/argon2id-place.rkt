#lang racket/base

(require crypto
         crypto/argon2
         racket/match
         racket/place)

(provide
 hasher-start)

(define-logger argon2id-hasher)

(define (hasher-start)
  (place ch
    (file-stream-buffer-mode (current-error-port) 'none)
    (parameterize ([crypto-factories (list argon2-factory)])
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
            [(list 'stop)
             (log-argon2id-hasher-debug "stopped")]

            [(list 'hash p out)
             (place-channel-put out (pwhash 'argon2id (string->bytes/utf-8 p) config))
             (loop)]

            [(list 'verify p h out)
             (place-channel-put out (pwhash-verify #f (string->bytes/utf-8 p) h))
             (loop)]))))))
