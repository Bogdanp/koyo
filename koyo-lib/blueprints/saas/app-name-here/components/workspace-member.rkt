#lang racket/base

(require deta
         koyo/database
         racket/contract/base
         threading
         "http.rkt")

(provide
 workspace-role/c
 (schema-out workspace-member)
 (contract-out
  [current-workspace-member
   (parameter/c (or/c #f workspace-member?))]
  [workspace-member-has-permission?
   (-> workspace-member? permission/c boolean?)]
  [lookup-workspace-member
   (-> database? id/c id/c (or/c #f workspace-member?))]
  [wrap-permissions
   (->* [procedure?]
        [#:permissions (listof permission/c)]
        (-> request? any/c ... response?))]))

(define workspace-role/c
  (or/c 'admin 'member))

(define permission/c
  (or/c 'read 'write))

(define-schema workspace-member
  #:table "workspace_members"
  ([workspace-id id/f]
   [user-id id/f]
   [(role 'member) symbol/f #:contract workspace-role/c]))

(define (workspace-member-has-permission? m permission)
  (case (workspace-member-role m)
    [(admin) #t]
    [(member) (and (member permission '(read)) #t)]
    [else (error 'workspace-member-has-permission? "not implemented")]))

(define (workspace-member-has-permissions? m permissions)
  (andmap (λ (permission) (workspace-member-has-permission? m permission)) permissions))

(define current-workspace-member
  (make-parameter #f))

(define (lookup-workspace-member db wid uid)
  (with-database-connection [conn db]
    (~> (from workspace-member #:as wm)
        (where (and (= wm.workspace-id ,wid)
                    (= wm.user-id ,uid)))
        (lookup conn _))))

(define ((wrap-permissions #:permissions [permissions '(read)] hdl) req . args)
  (if (workspace-member-has-permissions? (current-workspace-member) permissions)
      (apply hdl req args)
      (forbidden "Permission Denied")))
