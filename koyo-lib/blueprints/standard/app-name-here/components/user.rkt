#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         component
         db
         deta
         gregor
         koyo/database
         koyo/profiler
         koyo/random
         racket/contract
         racket/function
         racket/string
         threading
         "hash.rkt")

;; user ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (schema-out user))

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:contract non-empty-string? #:wrapper string-downcase]
   [password-hash string/f #:nullable]
   [(verified? #f) boolean/f]
   [(verification-code (generate-random-string)) string/f #:contract non-empty-string?]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f]))

(define/contract (set-user-password u p)
  (-> user? string? user?)
  (set-user-password-hash u (make-password-hash p)))

(define/contract (user-password-valid? u p)
  (-> user? string? boolean?)
  (hash-matches? (user-password-hash u) p))


;; user-manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 exn:fail:user-manager?
 exn:fail:user-manager:username-taken?

 make-user-manager
 user-manager?
 user-manager-lookup/id
 user-manager-lookup/username
 user-manager-create!
 user-manager-login
 user-manager-verify)

(struct exn:fail:user-manager exn:fail ())
(struct exn:fail:user-manager:username-taken exn:fail:user-manager ())

(struct user-manager (db)
  #:transparent
  #:methods gen:component
  [(define component-start identity)
   (define component-stop identity)])

(define/contract (make-user-manager db)
  (-> database? user-manager?)
  (user-manager db))

(define/contract (user-manager-create! um username password)
  (-> user-manager? string? string? user?)

  (define user
    (~> (make-user #:username username)
        (set-user-password password)))

  (with-handlers ([exn:fail:sql:constraint-violation?
                   (lambda _
                     (raise (exn:fail:user-manager:username-taken
                             (format "username '~a' is taken" username)
                             (current-continuation-marks))))])
    (with-database-transaction [conn (user-manager-db um)]
      (insert-one! conn user))))

(define/contract (user-manager-lookup/id um id)
  (-> user-manager? exact-positive-integer? (or/c false/c user?))
  (with-timing 'user-manager (format "(user-manager-lookup/id ~v)" id)
    (with-database-connection [conn (user-manager-db um)]
      (lookup conn (~> (from user #:as u)
                       (where (= u.id ,id)))))))

(define/contract (user-manager-lookup/username um username)
  (-> user-manager? string? (or/c false/c user?))
  (with-timing 'user-manager (format "(user-manager-lookup/username ~v)" username)
    (with-database-connection [conn (user-manager-db um)]
      (lookup conn (~> (from user #:as u)
                       (where (= u.username ,username)))))))

(define/contract (user-manager-login um username password)
  (-> user-manager? string? string? (or/c false/c user?))
  (with-timing 'user-manager "user-manager-login"
    (define user (user-manager-lookup/username um username))
    (and user (user-password-valid? user password) user)))

(define/contract (user-manager-verify um id verification-code)
  (-> user-manager? exact-positive-integer? string? void?)
  (with-timing 'user-manager "user-manager-verify"
    (void
     (with-database-transaction [conn (user-manager-db um)]
       (query-exec conn (~> (from user #:as u)
                            (update [verified? #t])
                            (where (and (= u.id ,id)
                                        (= u.verification-code ,verification-code)))))))))
