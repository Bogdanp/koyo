#lang racket/base

(require component
         net/tcp-unit
         racket/async-channel
         racket/contract
         racket/string
         racket/unix-socket-tcp-unit
         web-server/dispatchers/dispatch
         web-server/safety-limits
         web-server/web-server)

(provide
 make-server-factory
 server?)

(define-logger server)

(struct server (host port socket-path limits dispatcher stopper)
  #:transparent
  #:methods gen:component
  [(define (component-start a-server)
     (define ch (make-async-channel))
     (define stopper
       (serve #:dispatch (server-dispatcher a-server)
              #:listen-ip (server-host a-server)
              #:port (server-port a-server)
              #:tcp@ (cond
                       [(server-socket-path a-server)
                        => make-unix-socket-tcp@]

                       [else tcp@])
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
     (define socket-path (server-socket-path a-server))
     (when socket-path
       (with-handlers ([exn:fail:filesystem?
                        (lambda (e)
                          (log-server-warning "failed to delete server socket: ~a" (exn-message e)))])
         (delete-file socket-path)))
     (struct-copy server a-server [stopper #f]))])

(define/contract ((make-server-factory #:unix-socket [socket-path #f]
                                       #:host [host "127.0.0.1"]
                                       #:port [port 8000]
                                       #:limits [limits (make-safety-limits)]) dispatcher)
  (->* ()
       (#:unix-socket (or/c #f path-string?)
        #:host non-empty-string?
        #:port (integer-in 0 65535)
        #:limits safety-limits?)
       (-> dispatcher/c server?))
  (server host port socket-path limits dispatcher #f))
