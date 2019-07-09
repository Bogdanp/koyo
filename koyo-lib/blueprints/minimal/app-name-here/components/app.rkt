#lang racket/base

(require component
         koyo/continuation
         koyo/cors
         koyo/dispatch
         koyo/preload
         koyo/profiler
         koyo/session
         koyo/url
         racket/contract
         racket/function
         threading
         web-server/managers/lru
         web-server/servlet-dispatch
         "../pages/all.rkt")

(provide
 make-app
 app?
 app-dispatcher)

(struct app (dispatcher)
  #:methods gen:component
  [(define component-start identity)
   (define component-stop identity)])

(define/contract (make-app sessions)
  (-> session-manager? app?)
  (define-values (dispatch reverse-uri req-roles)
    (dispatch-rules+roles
     [("") home-page]
     [else not-found-page]))

  ;; Requests go up (starting from the last wrapper) and respones go down!
  (define (stack handler)
    (~> handler
        ((wrap-session sessions))
        (wrap-protect-continuations)
        (wrap-preload)
        (wrap-cors)
        (wrap-profiler)))

  (current-continuation-wrapper stack)
  (current-reverse-uri-fn reverse-uri)

  (define manager
    (make-threshold-LRU-manager (stack expired-page) (* 1024 1024 128)))

  (app (dispatch/servlet #:manager manager (stack dispatch))))
