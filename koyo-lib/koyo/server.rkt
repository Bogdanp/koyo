#lang racket/base

(require component
         net/tcp-sig
         net/tcp-unit
         racket/async-channel
         racket/contract/base
         racket/string
         racket/unit
         racket/unix-socket-tcp-unit
         web-server/dispatchers/dispatch
         web-server/safety-limits
         web-server/web-server)

(provide
 server?
 (contract-out
  [make-server-factory
   (->* []
        [#:unix-socket (or/c #f path-string?)
         #:limits safety-limits?
         #:host non-empty-string?
         #:port (integer-in 0 65535)
         #:tcp@ (unit/c (import) (export tcp^))]
        (-> dispatcher/c server?))]))

(define-logger server)

(struct server (host port tcp@ socket-path limits dispatcher stopper)
  #:transparent
  #:methods gen:component
  [(define (component-start a-server)
     (define exn-or-port-ch
       (make-async-channel))
     (define stopper
       (serve #:dispatch (server-dispatcher a-server)
              #:listen-ip (server-host a-server)
              #:port (server-port a-server)
              #:tcp@ (server-tcp@ a-server)
              #:safety-limits (server-limits a-server)
              #:confirmation-channel exn-or-port-ch))
     (define exn-or-port (sync exn-or-port-ch))
     (when (exn:fail? exn-or-port)
       (raise exn-or-port))
     (log-server-info
      "listening on ~a:~a"
      (server-host a-server)
      exn-or-port)
     (struct-copy server a-server [stopper stopper]))

   (define (component-stop a-server)
     ((server-stopper a-server))
     (define socket-path
       (server-socket-path a-server))
     (when socket-path
       (with-handlers ([exn:fail:filesystem?
                        (lambda (e)
                          (log-server-warning "failed to delete server socket: ~a" (exn-message e)))])
         (delete-file socket-path)))
     (struct-copy server a-server [stopper #f]))])

(define ((make-server-factory
          #:unix-socket [sock-path #f]
          #:limits [limits (make-safety-limits)]
          #:host [host "127.0.0.1"]
          #:port [port 8000]
          #:tcp@ [tcp@ (if sock-path (make-unix-socket-tcp@ sock-path) tcp@)])
         dispatcher)
  (server
   #;host host
   #;port port
   #;tcp@ tcp@
   #;socket-path sock-path
   #;limits limits
   #;dispatcher dispatcher
   #;stopper #f))
