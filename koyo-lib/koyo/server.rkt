#lang racket/base

(require component
         racket/contract
         racket/string
         web-server/dispatchers/dispatch
         web-server/web-server)

(provide
 make-server-factory
 server?)

(define-logger server)

(struct server (host port dispatcher stopper)
  #:transparent
  #:methods gen:component
  [(define (component-start a-server)
     (define stopper
       (serve #:dispatch (server-dispatcher a-server)
              #:listen-ip (server-host a-server)
              #:port (server-port a-server)))

     (log-server-info "listening on ~a:~a"
                      (server-host a-server)
                      (server-port a-server))
     (struct-copy server a-server [stopper stopper]))

   (define (component-stop a-server)
     ((server-stopper a-server))
     (struct-copy server a-server [stopper #f]))])

(define/contract ((make-server-factory #:host [host "127.0.0.1"]
                                       #:port [port 8000]) dispatcher)
  (->* ()
       (#:host non-empty-string?
        #:port (integer-in 0 65535))
       (-> dispatcher/c server?))
  (server host port dispatcher #f))
