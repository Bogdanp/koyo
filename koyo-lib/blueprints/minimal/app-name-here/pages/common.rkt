#lang racket/base

(require koyo/http
         net/url
         racket/contract
         web-server/http
         "../components/template.rkt")

(provide
 not-found-page
 expired-page)

(define/contract (not-found-page req)
  (-> request? response?)
  (page
   #:subtitle "Page Not Found"
   '(h1 "404 Not Found")))

(define/contract (expired-page req)
  (-> request? response?)
  (redirect-to (url->string (url-scrub (request-uri req)))))
