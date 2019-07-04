#lang racket/base

(require koyo/flash
         koyo/haml
         koyo/l10n
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
   #:subtitle (translate 'subtitle-not-found)
   (haml
    (.container
     (:h1 (translate 'subtitle-not-found))
     (:p (translate 'message-not-found))))))

(define/contract ((expired-page flashes) req)
  (-> flash-manager? (-> request? response?))

  (define (path/param-scrub pp)
    (path/param (path/param-path pp) null))

  (define the-url (request-uri req))
  (define scrubbed-url
    (let ([scrubbed-params (map path/param-scrub (url-path the-url))])
      (struct-copy url the-url [path scrubbed-params])))

  (flash flashes 'warning (translate 'message-session-expired))
  (redirect-to (url->string scrubbed-url)))
