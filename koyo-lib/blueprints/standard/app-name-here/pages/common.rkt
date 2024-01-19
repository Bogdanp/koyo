#lang racket/base

(require koyo/flash
         koyo/haml
         koyo/http
         koyo/l10n
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
   #:subtitle (translate 'subtitle-not-found)
   (haml
    (.container
     (:h1 (translate 'subtitle-not-found))
     (:p (translate 'message-not-found))))))

(define (expired-page req)
  (flash 'warning (translate 'message-session-expired))
  (redirect-to (url->string (url-scrub (request-uri req)))))
