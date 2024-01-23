#lang racket/base

(require racket/contract/base
         web-server/http
         "../components/template.rkt")

(provide
 (contract-out
  [home-page (-> request? response?)]))

(define (home-page _req)
  (page '(h1 "Hello!")))
