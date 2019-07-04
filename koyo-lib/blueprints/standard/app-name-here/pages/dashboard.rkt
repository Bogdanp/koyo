#lang racket/base

(require koyo/haml
         racket/contract
         web-server/http
         "../components/auth.rkt"
         "../components/user.rkt"
         "../components/template.rkt")

(provide
 dashboard-page)

(define/contract (dashboard-page req)
  (-> request? response?)
  (page
   (haml
    (.container
     (:h1 "Hello World!")))))
