#lang racket/base

(require component
         racket/async-channel
         racket/contract
         racket/string
         web-server/dispatchers/dispatch
         web-server/safety-limits
         web-server/web-server)

(provide
 make-server-factory
 server?)

(define-logger server)

(struct server (host port limits dispatcher stopper)
  #:transparent
  #:methods gen:component
  [(define (component-start a-server)
     (define ch (make-async-channel))
     (define stopper
       (serve #:dispatch (server-dispatcher a-server)
              #:listen-ip (server-host a-server)
              #:port (server-port a-server)
              #:safety-limits (server-limits a-server)
              #:confirmation-channel ch))

     (define maybe-exn (sync ch))
     (when (exn:fail? maybe-exn)
       (raise maybe-exn))

     (log-server-info "listening on ~a:~a"
                      (server-host a-server)
                      (server-port a-server))
     (struct-copy server a-server [stopper stopper]))

   (define (component-stop a-server)
     ((server-stopper a-server))
     (struct-copy server a-server [stopper #f]))])

(define/contract ((make-server-factory #:host [host "127.0.0.1"]
                                       #:port [port 8000]
                                       #:limits [limits (make-safety-limits)]) dispatcher)
  (->* ()
       (#:host non-empty-string?
        #:port (integer-in 0 65535)
        #:limits safety-limits?)
       (-> dispatcher/c server?))
  (server host port limits dispatcher #f))
