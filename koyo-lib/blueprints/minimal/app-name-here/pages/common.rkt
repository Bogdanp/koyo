#lang racket/base

(require koyo/http
         net/url
         racket/contract/base
         web-server/http
         "../components/template.rkt")

(provide
 (contract-out
  [not-found-page (-> request? response?)]
  [expired-page (-> request? response?)]))

(define (not-found-page _req)
  (page
   #:subtitle "Page Not Found"
   '(h1 "404 Not Found")))

(define (expired-page req)
  (redirect-to (url->string (url-scrub (request-uri req)))))
