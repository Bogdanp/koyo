#lang racket/base

(require koyo/flash
         koyo/http
         koyo/l10n
         net/url
         racket/contract/base
         web-server/http)

(provide
 (contract-out
  [expired-page (-> request? response?)]))

(define (expired-page req)
  (flash 'warning (translate 'message-session-expired))
  (redirect-to (url->string (url-scrub (request-uri req)))))
