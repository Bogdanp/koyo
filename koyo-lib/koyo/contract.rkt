#lang racket/base

(require racket/contract
         web-server/http
         web-server/servlet/servlet-structs)

(provide
 middleware/c)

(define middleware/c
  (-> procedure? (-> request? any/c ... can-be-response?)))
