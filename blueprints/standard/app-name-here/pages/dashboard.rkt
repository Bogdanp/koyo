#lang racket/base

(require racket/contract/base
         web-server/http
         "../components/auth.rkt"
         "../components/user.rkt"
         "../components/template.rkt")

(provide
 (contract-out
  [dashboard-page (-> request? response?)]))

(define (dashboard-page req)
  (page
   (container
    '(h1 "Dashboard")
    `(p "Hi " ,(user-username (current-user)) "!"))))
