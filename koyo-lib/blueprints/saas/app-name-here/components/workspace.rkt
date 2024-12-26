#lang racket/base

(require db
         deta
         gregor
         koyo/database
         koyo/guard
         racket/contract/base
         threading
         "auth.rkt"
         "http.rkt"
         "workspace-member.rkt")

(provide
 (schema-out workspace)
 (contract-out
  [current-workspace (parameter/c (or/c #f workspace?))]
  [current-workspace-id (-> id/c)]
  [lookup-workspace-by-id (-> database? id/c (or/c #f workspace?))]
  [lookup-workspace-by-key (-> database? string? (or/c #f workspace?))]
  [lookup-workspace-by-slug (-> database? id/c string? (or/c #f workspace?))]
  [get-workspaces-by-user (-> database? id/c (listof workspace?))]
  [wrap-workspace (-> database? (-> procedure? (-> request? id/c any/c ... response?)))]))

(define-schema workspace
  ([id id/f #:primary-key #:auto-increment]
   [(name "Unnamed Workspace") string/f]
   [(slug (slugify name)) string/f]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f])
  #:pre-persist-hook
  (lambda (e)
    (set-workspace-updated-at e (now/moment))))

(define current-workspace
  (make-parameter #f))
(define current-workspace-id
  (compose1 workspace-id current-workspace))

(define (slugify name)
  (~> (string-downcase name)
      (regexp-replace* #rx"[^a-zA-Z0-9-]+" _ "-")))

(define (lookup-workspace-by-id db id)
  (with-database-connection [conn db]
    (~> (from workspace #:as w)
        (where (= w.id ,id))
        (lookup conn _))))

(define (lookup-workspace-by-key db key)
  (with-database-connection [conn db]
    (~> (from workspace #:as w)
        (join "workspace_keys" #:as wk #:on (= w.id wk.workspace-id))
        (where (= wk.key ,key))
        (lookup conn _))))

(define (lookup-workspace-by-slug db uid slug)
  (with-database-connection [conn db]
    (~> (from workspace #:as w)
        (join "workspace_members" #:as wm #:on (= w.id wm.workspace-id))
        (where (and (= wm.user-id ,uid)
                    (= w.slug ,slug)))
        (lookup conn _))))

(define (get-workspaces-by-user db uid)
  (with-database-connection [conn db]
    (~> (from workspace #:as w)
        (join "workspace_members" #:as wm #:on (= w.id wm.workspace-id))
        (where (= wm.user-id ,uid))
        (query-entities conn _))))

(define (((wrap-workspace db) hdl) req workspace-id . args)
  (with-guard not-found
    (define-values (w m)
      (with-database-transaction [_ db]
        (define w (guard (lookup-workspace-by-id db workspace-id))) ;; noqa
        (values w (guard (lookup-workspace-member db workspace-id (current-user-id))))))
    (parameterize ([current-workspace w]
                   [current-workspace-member m])
      (apply hdl req args))))
