#lang racket/base

(require racket/contract
         web-server/http
         "../components/template.rkt")

(provide
 home-page)

(define/contract (home-page req)
  (-> request? response?)
  (page '(h1 "Hello!")))
