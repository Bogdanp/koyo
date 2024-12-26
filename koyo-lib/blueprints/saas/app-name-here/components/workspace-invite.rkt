#lang racket/base

(require db
         deta
         koyo/database
         koyo/random
         racket/contract/base
         racket/string
         threading
         "workspace-member.rkt")

(provide
 (schema-out workspace-invite)
 (contract-out
  [invite-workspace-member (-> database? id/c string? workspace-invite?)]))

(define-schema workspace-invite
  #:table "workspace_invites"
  ([id id/f #:primary-key #:auto-increment]
   [workspace-id id/f]
   [email string/f]
   [(token (generate-random-string)) string/f]
   [(role 'member) symbol/f #:contract workspace-role/c]))

(define (invite-workspace-member db wid email [role 'member])
  (let ([email (string-downcase (string-trim email))])
    (with-database-transaction [conn db]
      (~> (from workspace-invite #:as wi)
          (where (and (= wi.workspace-id ,wid)
                      (= wi.email ,email)))
          (delete)
          (query-exec conn _))
      (~> (make-workspace-invite
           #:workspace-id wid
           #:email email
           #:role role)
          (insert-one! conn _)))))
