#lang racket/base

(require racket/contract
         racket/string
         web-server/http
         "profiler.rkt"
         "random.rkt"
         "session.rkt")

(provide
 request-csrf-token
 request-protected?

 current-csrf-error-handler
 current-csrf-token-generator
 current-csrf-token-reader
 current-csrf-token
 wrap-csrf)

(define session-key 'csrf.token)

(define safe-methods
  '(#"GET" #"HEAD" #"OPTIONS" #"TRACE"))

(define (request-protected? req)
  (not (member (request-method req) safe-methods)))

(define (request-csrf-token req)
  (or (let ([header (headers-assq* #"x-csrf-token" (request-headers/raw req))])
        (and header (bytes->string/utf-8 (header-value header))))
      (let ([binding (bindings-assq #"csrf-token" (request-bindings/raw req))])
        (and binding (bytes->string/utf-8 (binding:form-value binding))))))

(define (error-handler req)
  (response/xexpr
   #:preamble #"<doctype html>"
   #:code 403
   #:message #"Forbidden"
   '(div
     (h1 "Error")
     (p "Invalid CSRF token."))))

(define/contract current-csrf-token-generator
  (parameter/c (-> non-empty-string?))
  (make-parameter generate-random-string))

(define/contract current-csrf-token
  (parameter/c (or/c false/c non-empty-string?))
  (make-parameter #f))

(define/contract current-csrf-token-reader
  (parameter/c (-> request? (or/c false/c non-empty-string?)))
  (make-parameter request-csrf-token))

(define/contract current-csrf-error-handler
  (parameter/c (-> request? response?))
  (make-parameter error-handler))

(define/contract (((wrap-csrf sessions) handler) req . args)
  (-> session-manager? (-> procedure? (-> request? any/c ... response?)))
  (with-timing 'csrf "wrap-csrf"
    (session-manager-update! sessions
                             session-key
                             (lambda (token-from-session)
                               (or token-from-session ((current-csrf-token-generator)))) #f)

    (define csrf-token
      (session-manager-ref sessions session-key))

    (parameterize [(current-csrf-token csrf-token)]
      (cond
        [(request-protected? req)
         (if (equal? ((current-csrf-token-reader) req) csrf-token)
             (apply handler req args)
             (error-handler req))]

        [else
         (apply handler req args)]))))
