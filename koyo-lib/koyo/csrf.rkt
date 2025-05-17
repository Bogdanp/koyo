#lang racket/base

(require racket/contract/base
         racket/string
         web-server/http
         "contract.rkt"
         "profiler.rkt"
         "random.rkt"
         "session.rkt")

(provide
 request-csrf-token
 request-protected?

 (contract-out
  [current-csrf-error-handler (parameter/c (-> request? response?))]
  [current-csrf-token-generator (parameter/c (-> non-empty-string?))]
  [current-csrf-token-reader (parameter/c (-> request? (or/c #f non-empty-string?)))]
  [current-csrf-token (parameter/c (or/c #f non-empty-string?))]
  [wrap-csrf (-> session-manager? middleware/c)]))

(define session-key 'csrf.token)

(define safe-methods
  '(#"GET" #"HEAD" #"OPTIONS" #"TRACE"))

(define (request-protected? req)
  (not (member (request-method req) safe-methods)))

(define (request-csrf-token req)
  (or (let ([h (headers-assq* #"x-csrf-token" (request-headers/raw req))])
        (and h (bytes->string/utf-8 (header-value h))))
      (let ([b (bindings-assq #"csrf-token" (request-bindings/raw req))])
        (and b (bytes->string/utf-8 (binding:form-value b))))))

(define (error-handler _req)
  (response/xexpr
   #:preamble #"<!doctype html>"
   #:code 403
   #:message #"Forbidden"
   '(div
     (h1 "Error")
     (p "Invalid CSRF token."))))

(define current-csrf-token-generator
  (make-parameter generate-random-string))

(define current-csrf-token
  (make-parameter #f))

(define current-csrf-token-reader
  (make-parameter request-csrf-token))

(define current-csrf-error-handler
  (make-parameter error-handler))

(define (((wrap-csrf sessions) handler) req . args)
  (with-timing 'csrf "wrap-csrf"
    (session-manager-update!
     sessions
     session-key
     (lambda (token-from-session)
       (or token-from-session ((current-csrf-token-generator))))
     #;default #f)
    (define csrf-token (session-manager-ref sessions session-key))
    (parameterize [(current-csrf-token csrf-token)]
      (cond
        [(request-protected? req)
         (if (equal? ((current-csrf-token-reader) req) csrf-token)
             (apply handler req args)
             ((current-csrf-error-handler) req))]

        [else
         (apply handler req args)]))))
