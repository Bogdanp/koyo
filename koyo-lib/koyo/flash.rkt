#lang racket/base

(require component
         racket/contract
         web-server/servlet
         "profiler.rkt"
         "session.rkt")

;; Flash manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-flash-messages
 current-flash-manager
 make-flash-manager
 flash-manager?
 flash)

(define session-key 'flash.messages)

(struct flash-manager (session-manager)
  #:methods gen:component
  [(define component-start values)
   (define component-stop values)])

(define/contract (make-flash-manager sessions)
  (-> session-manager? flash-manager?)
  (flash-manager sessions))

(define current-flash-manager
  (make-parameter #f))

(define/contract current-flash-messages
  (parameter/c (listof (cons/c symbol? string?)))
  (make-parameter null))

(define flash
  (case-lambda
    [(key message)
     (flash (current-flash-manager) key message)]

    [(fm key message)
     (with-timing 'flash "flash"
       (session-manager-update! (flash-manager-session-manager fm)
                                session-key
                                (lambda (flashes)
                                  (cons (cons key message) flashes))
                                null))]))

(module+ private
  (provide current-flash-manager))


;; Middleware ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 wrap-flash)

(define/contract (((wrap-flash fm) handler) req)
  (-> flash-manager?
      (-> (-> request? can-be-response?)
          (-> request? can-be-response?)))

  (with-timing 'flash "wrap-flash"
    (define sessions (flash-manager-session-manager fm))
    (define flash-messages (session-manager-ref sessions session-key null))
    (session-manager-remove! sessions session-key)

    (parameterize ([current-flash-manager fm]
                   [current-flash-messages flash-messages])
      (handler req))))
