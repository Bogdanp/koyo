#lang racket/base

(require component
         db
         deta
         gregor
         koyo/database
         koyo/hasher
         koyo/profiler
         koyo/random
         racket/contract
         racket/string
         threading)

;; user ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (schema-out user))

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:contract non-empty-string? #:wrapper string-downcase]
   [(password-hash "") string/f]
   [(verified? #f) boolean/f]
   [(verification-code (generate-random-string)) string/f #:contract non-empty-string?]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f])

  #:pre-persist-hook
  (lambda (u)
    (set-user-updated-at u (now/moment))))

(define (set-password um u p)
  (set-user-password-hash u (hasher-make-hash (user-manager-hasher um) p)))

(define (password-valid? um u p)
  (hasher-hash-matches? (user-manager-hasher um) (user-password-hash u) p))


;; password reset ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-schema password-reset
  #:table "password_reset_requests"
  ([user-id id/f #:unique]
   [ip-address string/f #:contract non-empty-string?]
   [user-agent string/f #:contract non-empty-string?]
   [(token (generate-random-string)) string/f #:contract non-empty-string?]
   [(expires-at (+days (now/moment) 1)) datetime-tz/f]))


;; user-manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 exn:fail:user-manager?
 exn:fail:user-manager:username-taken?

 make-user-manager
 user-manager?
 user-manager-lookup/id
 user-manager-lookup/username
 user-manager-create!
 user-manager-create-reset-token!
 user-manager-login
 user-manager-verify!
 user-manager-reset-password!)

(struct exn:fail:user-manager exn:fail ())
(struct exn:fail:user-manager:username-taken exn:fail:user-manager ())

(struct user-manager (db hasher)
  #:transparent)

(define/contract (make-user-manager db h)
  (-> database? hasher? user-manager?)
  (user-manager db h))

(define/contract (user-manager-create! um username password)
  (-> user-manager? string? string? user?)

  (define user
    (~> (make-user #:username username)
        (set-password um _ password)))

  (with-handlers ([exn:fail:sql:constraint-violation?
                   (lambda _
                     (raise (exn:fail:user-manager:username-taken
                             (format "username '~a' is taken" username)
                             (current-continuation-marks))))])
    (with-database-transaction [conn (user-manager-db um)]
      (insert-one! conn user))))

(define/contract (user-manager-create-reset-token! um
                                                   #:username username
                                                   #:ip-address ip-address
                                                   #:user-agent user-agent)
  (-> user-manager?
      #:username non-empty-string?
      #:ip-address non-empty-string?
      #:user-agent non-empty-string?
      (values (or/c false/c user?)
              (or/c false/c string?)))
  (with-timing 'user-manager "user-manager-create-reset-token!"
    (with-database-transaction [conn (user-manager-db um)]
      (cond
        [(user-manager-lookup/username um username)
         => (lambda (user)
              (query-exec conn (delete (~> (from password-reset #:as pr)
                                           (where (= pr.user-id ,(user-id user))))))

              (values user
                      (~> (make-password-reset #:user-id (user-id user)
                                               #:ip-address ip-address
                                               #:user-agent user-agent)
                          (insert-one! conn _)
                          (password-reset-token))))]

        [else
         (values #f #f)]))))

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
                       (where (= u.username ,(string-downcase username))))))))

(define/contract (user-manager-login um username password)
  (-> user-manager? string? string? (or/c false/c user?))
  (with-timing 'user-manager "user-manager-login"
    (define user (user-manager-lookup/username um username))
    (and user (password-valid? um user password) user)))

(define/contract (user-manager-verify! um id verification-code)
  (-> user-manager? exact-positive-integer? string? void?)
  (with-timing 'user-manager "user-manager-verify!"
    (void
     (with-database-transaction [conn (user-manager-db um)]
       (query-exec conn (~> (from user #:as u)
                            (update [verified? #t])
                            (where (and (= u.id ,id)
                                        (= u.verification-code ,verification-code)))))))))

(define/contract (user-manager-reset-password! um
                                               #:user-id user-id
                                               #:token token
                                               #:password password)
  (-> user-manager?
      #:user-id id/c
      #:token non-empty-string?
      #:password non-empty-string?
      boolean?)
  (with-timing 'user-manager "user-manager-reset-password!"
    (with-database-transaction [conn (user-manager-db um)]
      (cond
        [(lookup-password-reset conn user-id token)
         => (lambda (_pr)
              (begin0 #t
                (clear-password-reset! conn user-id)
                (and~> (lookup conn
                               (~> (from user #:as u)
                                   (where (= u.id ,user-id))))
                       (set-password um _ password)
                       (update! conn _))))]


        [else #f]))))

(define (lookup-password-reset conn user-id token)
  (lookup conn (~> (from password-reset #:as pr)
                   (where (and (= pr.user-id ,user-id)
                               (= pr.token ,token)
                               (> pr.expires-at (now)))))))

(define (clear-password-reset! conn user-id)
  (query-exec conn (~> (from password-reset #:as pr)
                       (where (= pr.user-id ,user-id))
                       (delete))))
