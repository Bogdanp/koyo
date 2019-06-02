#lang racket/base

(require net/url
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

  (define the-uri (request-uri req))
  (define (path/param-scrub pp)
    (path/param (path/param-path pp) null))

  (define scrubbed-url
    (let ([scrubbed-path (map path/param-scrub (url-path the-uri))])
      (struct-copy url (request-uri req)
                   [path scrubbed-path])))

  (redirect-to (url->string scrubbed-url)))
