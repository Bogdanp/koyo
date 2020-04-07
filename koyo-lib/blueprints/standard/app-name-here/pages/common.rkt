#lang racket/base

(require koyo/flash
         koyo/haml
         koyo/http
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

(define/contract (expired-page req)
  (-> request? response?)
  (flash 'warning (translate 'message-session-expired))
  (redirect-to (url->string (url-scrub (request-uri req)))))
