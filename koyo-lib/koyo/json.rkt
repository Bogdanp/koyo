#lang racket/base

(require json
         racket/contract
         web-server/http)

(provide
 response/json)

(define/contract (response/json body
                                #:code [code 200]
                                #:headers [headers '()])
  (->* (jsexpr?)
       (#:code exact-positive-integer?
        #:headers (listof header?))
       response?)

  (response/output
   #:code code
   #:headers headers
   #:mime-type #"application/json; charset=utf-8"
   (lambda (out)
     (write-json body out))))
