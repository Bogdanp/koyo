#lang racket/base

(require koyo/flash
         net/url
         racket/contract/base
         web-server/http
         "../components/template.rkt")

(provide
 (contract-out
  [not-found-page (-> request? response?)]
  [expired-page (-> flash-manager? (-> request? response?))]))

(define (not-found-page req)
  (page
   #:subtitle "Page Not Found"
   (container
    '(h1 "Page Not Found")
    '(p "Couldn't find nothing, " (em "nothing") " I tell ya!"))))

(define ((expired-page flashes) req)
  (define (path/param-scrub pp)
    (path/param (path/param-path pp) null))

  (define scrubbed-url
    (struct-copy url (request-uri req) [path (map path/param-scrub (url-path (request-uri req)))]))

  (flash flashes 'warning "Sorry! Your session expired. Please try again.")
  (redirect-to (url->string scrubbed-url)))
