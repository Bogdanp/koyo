#lang racket/base

(require json
         racket/contract/base
         web-server/http)

(provide
 (contract-out
  [response/json
   (->* [jsexpr?]
        [#:code exact-positive-integer?
         #:headers (listof header?)]
        response?)]))

(define (response/json body
                       #:code [code 200]
                       #:headers [headers '()])
  (response/output
   #:code code
   #:headers headers
   #:mime-type #"application/json; charset=utf-8"
   (lambda (out)
     (write-json body out))))
