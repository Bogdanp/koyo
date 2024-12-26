#lang racket/base

(require koyo/http
         racket/contract/base
         web-server/http)

(provide
 (all-from-out koyo/http web-server/http)
 (contract-out
  [bad-request (->* [] [string?] response?)]
  [forbidden (->* [] [string?] response?)]
  [not-found (->* [] [string?] response?)]))

(define (bad-request [message "Bad Request"])
  (response/jsexpr
   #:code 400
   (hasheq 'error (hasheq 'message message))))

(define (forbidden [message "Forbidden"])
  (response/jsexpr
   #:code 403
   (hasheq 'error (hasheq 'message message))))

(define (not-found [message "Not Found"])
  (response/jsexpr
   #:code 404
   (hasheq 'error (hasheq 'message message))))
