#lang racket/base

(require deta
         koyo/database
         koyo/guard
         racket/contract/base
         threading
         "../../components/auth.rkt"
         "../../components/http.rkt"
         "../../components/json.rkt"
         "../../components/workspace-member.rkt"
         "../../components/workspace.rkt"
         "../common.rkt")

(provide
 (contract-out
  [create-workspace-page (-> database? (-> request? response?))]
  [workspaces-page (-> database? (-> request? response?))]))

(define ((create-workspace-page db) req)
  (with-guard (λ () (bad-request))
    (define data (guard (request-json req)))
    (define name (guard (if-null (hash-ref data 'name #f) #f)))
    (define slug (guard (if-null (hash-ref data 'slug #f) #f)))
    (guard (valid-slug? slug))
    (with-handlers ([exn:fail:sql:constraint-violation?
                     (lambda (_)
                       (bad-request "This workspace slug is taken."))])
      (define the-workspace
        (with-database-transaction [conn db]
          (define the-workspace ;; noqa
            (~> (make-workspace
                 #:name name
                 #:slug slug)
                (insert-one! conn _)))
          (begin0 the-workspace
            (~> (make-workspace-member
                 #:workspace-id (workspace-id the-workspace)
                 #:user-id (current-user-id)
                 #:role 'admin)
                (insert-one! conn _)))))
      (response/jsexpr
       (->jsexpr the-workspace)))))

(define ((workspaces-page db) _req)
  (response/jsexpr (->jsexpr (get-workspaces-by-user db (current-user-id)))))

(define (valid-slug? s)
  (regexp-match-exact? #rx"[a-z0-9-]+" s))
