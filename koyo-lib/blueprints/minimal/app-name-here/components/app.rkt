#lang racket

(require koyo
         threading
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/managers/lru
         web-server/servlet-dispatch
         "../pages/all.rkt")

(provide
 make-app
 app?
 app-dispatcher)

(struct app (dispatcher))

(define/contract (make-app debug? sessions)
  (-> boolean? session-manager? app?)
  (define-values (dispatch reverse-uri _req-roles)
    (dispatch-rules+roles
     [("") home-page]))

  ;; Requests go up (starting from the last wrapper) and respones go down!
  (define (stack handler)
    (~> handler
        ((wrap-session sessions))
        (wrap-protect-continuations)
        (wrap-preload)
        (wrap-cors)
        (wrap-profiler)))

  (when debug?
    (current-continuation-key-cookie-secure? #f))
  (current-continuation-wrapper stack)
  (current-reverse-uri-fn reverse-uri)

  (define manager
    (make-threshold-LRU-manager (stack expired-page) (* 1024 1024 512)))

  (app
   (sequencer:make
    (dispatch/servlet #:manager manager (stack dispatch))
    (dispatch/servlet #:manager manager (stack not-found-page)))))
