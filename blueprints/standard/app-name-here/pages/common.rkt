#lang racket/base

(require koyo/flash
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
   (container
    '(h1 "Page Not Found")
    '(p "Couldn't find nothing, " (em "nothing") " I tell ya!"))))

(define/contract ((expired-page flashes) req)
  (-> flash-manager? (-> request? response?))

  (define (path/param-scrub pp)
    (path/param (path/param-path pp) null))

  (define the-url (request-uri req))
  (define scrubbed-url
    (let ([scrubbed-params (map path/param-scrub (url-path the-url))])
      (struct-copy url the-url [path scrubbed-params])))

  (flash flashes 'warning "Sorry! Your session expired. Please try again.")
  (redirect-to (url->string scrubbed-url)))
