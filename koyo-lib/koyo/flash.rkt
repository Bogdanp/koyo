#lang racket/base

(require racket/contract/base
         "contract.rkt"
         "profiler.rkt"
         "session.rkt")

;; Flash manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 flash-manager?
 (contract-out
  [current-flash-messages (parameter/c (listof (cons/c symbol? string?)))]
  [current-flash-manager (parameter/c (or/c #f flash-manager?))]
  [make-flash-manager (-> session-manager? flash-manager?)]
  [flash (case->
          (-> symbol? string? void?)
          (-> flash-manager? symbol? string? void?))]))

(define session-key 'flash.messages)

(struct flash-manager (sessions))

(define (make-flash-manager sessions)
  (flash-manager sessions))

(define current-flash-manager
  (make-parameter #f))

(define current-flash-messages
  (make-parameter null))

(define flash
  (case-lambda
    [(key message)
     (flash (current-flash-manager) key message)]

    [(fm key message)
     (with-timing 'flash "flash"
       (session-manager-update!
        (flash-manager-sessions fm)
        session-key
        (lambda (flashes)
          (cons (cons key message) flashes))
        null))]))


;; Middleware ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [wrap-flash (-> flash-manager? middleware/c)]))

(define (((wrap-flash fm) handler) req . args)
  (with-timing 'flash "wrap-flash"
    (define sessions (flash-manager-sessions fm))
    (define flash-messages (session-manager-ref sessions session-key null))
    (session-manager-remove! sessions session-key)

    (parameterize ([current-flash-manager fm]
                   [current-flash-messages flash-messages])
      (apply handler req args))))
