#lang racket/base

(require db
         deta
         koyo/database
         koyo/guard
         racket/contract/base
         threading
         "../../components/auth.rkt"
         "../../components/http.rkt"
         "../../components/json.rkt"
         "../../components/mail.rkt"
         "../../components/user.rkt"
         "../../components/workspace-invite.rkt"
         "../../components/workspace-member.rkt"
         "../../components/workspace.rkt"
         "../common.rkt")

(provide
 (contract-out
  [workspace-members-page (-> database? (-> request? response?))]
  [invite-workspace-member-page (-> database? mailer? (-> request? response?))]
  [workspace-invite-page (-> database? (-> request? id/c string? response?))]
  [join-workspace-page (-> database? (-> request? id/c string? response?))]
  [delete-workspace-invite-page (-> database? (-> request? id/c response?))]
  [delete-workspace-member-page (-> database? (-> request? id/c response?))]))

(define status/c
  (or/c 'pending 'active))

(define-schema member-data
  #:virtual
  ([id id/f]
   [email string/f]
   [role symbol/f]
   [status symbol/f #:contract status/c]))

(define ((workspace-members-page db) _req)
  (response/jsexpr
   (with-database-connection [conn db]
     (~> (from workspace-member #:as wm)
         (join user #:as u #:on (= u.id wm.user-id))
         (where (= wm.workspace-id ,(current-workspace-id)))
         (select u.id u.username wm.role "active")
         (union (~> (from workspace-invite #:as wi)
                    (where (= wi.workspace-id ,(current-workspace-id)))
                    (select wi.id wi.email wi.role "pending")))
         (project-onto member-data-schema)
         (query-entities conn _)
         (->jsexpr)))))

(define ((invite-workspace-member-page db mailer) req)
  (with-guard bad-request
    (define data (guard (request-json req)))
    (define email (guard (if-null (hash-ref data 'email #f) #f)))
    (with-database-transaction [_ db]
      (define wi (invite-workspace-member db (current-workspace-id) email))
      (send-workspace-invite-email mailer (current-workspace) wi))
    (response/empty)))

(define-schema workspace-data
  #:virtual
  ([workspace string/f]))

(define ((workspace-invite-page db) _req workspace-id token)
  (with-guard not-found
    (define data
      (with-database-connection [conn db]
        (~> (from workspace-invite #:as wi)
            (join workspace #:as w #:on (= w.id wi.workspace-id))
            (where (and (= wi.workspace-id ,workspace-id)
                        (= wi.token ,token)))
            (select w.name)
            (project-onto workspace-data-schema)
            (lookup conn _)
            (guard))))
    (response/jsexpr
     (->jsexpr data))))

(define ((join-workspace-page db) _req workspace-id token)
  (with-guard forbidden
    (with-database-transaction [conn db]
      (define invite
        (~> (from workspace-invite #:as wi)
            (where (and (= wi.workspace-id ,workspace-id)
                        (= wi.token ,token)))
            (lookup conn _)
            (guard)))
      (unless (lookup-workspace-member db workspace-id (current-user-id))
        (~> (make-workspace-member
             #:workspace-id workspace-id
             #:user-id (current-user-id)
             #:role (workspace-invite-role invite))
            (insert-one! conn _)))
      (delete-one! conn invite))
    (response/empty)))

(define ((delete-workspace-invite-page db) _req id)
  (with-database-connection [conn db]
    (~> (from workspace-invite #:as wi)
        (where (and (= wi.workspace-id ,(current-workspace-id))
                    (= wi.id ,id)))
        (delete)
        (query-exec conn _)))
  (response/empty))

(define ((delete-workspace-member-page db) _req uid)
  (unless (= uid (current-user-id))
    (with-database-connection [conn db]
      (~> (from workspace-member #:as wm)
          (where (and (= wm.workspace-id ,(current-workspace-id))
                      (= wm.user-id ,uid)))
          (delete)
          (query-exec conn _))))
  (response/empty))
