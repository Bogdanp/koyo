#lang racket/base

(require crypto
         crypto/argon2
         koyo/logging
         racket/match
         racket/place)

(provide hasher-start)

(define-logger hasher)

;; https://password-hashing.net/argon2-specs.pdf
(define CONFIG
  '((p 2)    ;; parallelism, adjust according to number of cores
    (t 256)  ;; iterations, adjust based on duration
    (m 2048) ;; memory per p in kb, adjust based on available memory
    ))

(define (hasher-start)
  (place ch
    (file-stream-buffer-mode (current-error-port) 'none)

    (start-logger #:levels '((hasher . debug))
                  #:output-port (current-error-port))

    (parameterize ([crypto-factories (list argon2-factory)])
      (let loop ()
        (with-handlers ([exn?
                         (lambda (e)
                           (log-hasher-error "unhandled error: ~a" (exn-message e)))])

          (log-hasher-debug "ready for messages")
          (match (place-channel-get ch)
            [(list 'hash p)
             (log-hasher-debug "received 'hash message")
             (place-channel-put ch (pwhash 'argon2id (string->bytes/utf-8 p) CONFIG))]

            [(list 'verify p h)
             (log-hasher-debug "received 'verify message")
             (place-channel-put ch (pwhash-verify #f (string->bytes/utf-8 p) h))]

            [m
             (log-hasher-error "unhandled message: ~a" m)]))

        (loop)))))
