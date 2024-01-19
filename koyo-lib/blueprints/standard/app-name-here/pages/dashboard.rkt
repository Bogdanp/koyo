#lang racket/base

(require koyo/haml
         racket/contract/base
         web-server/http
         "../components/template.rkt")

(provide
 (contract-out
  [dashboard-page (-> request? response?)]))

(define (dashboard-page _req)
  (page
   (haml
    (.container
     (:h1 "Hello World!")))))
