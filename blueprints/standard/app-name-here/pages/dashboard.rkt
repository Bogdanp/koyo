#lang racket/base

(require racket/contract
         web-server/http
         "../components/user.rkt"
         "../components/template.rkt")

(provide
 dashboard-page)

(define/contract (dashboard-page req)
  (-> request? response?)
  (page
   (container
    '(h1 "Dashboard")
    `(p "Hi " ,(user-username (current-user)) "!"))))
