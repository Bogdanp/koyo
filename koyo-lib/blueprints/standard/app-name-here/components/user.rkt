#lang racket/base

(require component
         db
         deta
         gregor
         koyo/database
         koyo/hasher
         koyo/profiler
         koyo/random
         racket/contract/base
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

 (contract-out
  [make-user-manager (-> database? hasher? user-manager?)]
  [user-manager? (-> any/c boolean?)]
  [user-manager-create! (-> user-manager? string? string? user?)]
  [user-manager-create-reset-token! (-> user-manager?
                                        #:username non-empty-string?
                                        #:ip-address non-empty-string?
                                        #:user-agent non-empty-string?
                                        (values (or/c #f user?)
                                                (or/c #f string?)))]
  [user-manager-lookup/id (-> user-manager? exact-positive-integer? (or/c #f user?))]
  [user-manager-lookup/username (-> user-manager? string? (or/c #f user?))]
  [user-manager-login (-> user-manager? string? string? (or/c #f user?))]
  [user-manager-verify! (-> user-manager? exact-positive-integer? string? void?)]
  [user-manager-reset-password! (-> user-manager?
                                    #:user-id id/c
                                    #:token non-empty-string?
                                    #:password non-empty-string?
                                    boolean?)]))

(struct exn:fail:user-manager exn:fail ())
(struct exn:fail:user-manager:username-taken exn:fail:user-manager ())

(struct user-manager (db hasher)
  #:transparent)

(define (make-user-manager db h)
  (user-manager db h))

(define (user-manager-create! um username password)
  (define the-user
    (~> (make-user #:username username)
        (set-password um _ password)))

  (with-handlers ([exn:fail:sql:constraint-violation?
                   (lambda (_e)
                     (raise (exn:fail:user-manager:username-taken
                             (format "username '~a' is taken" username)
                             (current-continuation-marks))))])
    (with-database-transaction [conn (user-manager-db um)]
      (insert-one! conn the-user))))

(define (user-manager-create-reset-token! um
                                          #:username username
                                          #:ip-address ip-address
                                          #:user-agent user-agent)
  (with-timing 'user-manager "user-manager-create-reset-token!"
    (with-database-transaction [conn (user-manager-db um)]
      (cond
        [(user-manager-lookup/username um username)
         => (lambda (u)
              (query-exec conn (delete (~> (from password-reset #:as pr)
                                           (where (= pr.user-id ,(user-id u))))))
              (values u (~> (make-password-reset #:user-id (user-id u)
                                                 #:ip-address ip-address
                                                 #:user-agent user-agent)
                            (insert-one! conn _)
                            (password-reset-token))))]

        [else
         (values #f #f)]))))

(define (user-manager-lookup/id um id)
  (with-timing 'user-manager (format "(user-manager-lookup/id ~v)" id)
    (with-database-connection [conn (user-manager-db um)]
      (lookup conn (~> (from user #:as u)
                       (where (= u.id ,id)))))))

(define (user-manager-lookup/username um username)
  (with-timing 'user-manager (format "(user-manager-lookup/username ~v)" username)
    (with-database-connection [conn (user-manager-db um)]
      (lookup conn (~> (from user #:as u)
                       (where (= u.username ,(string-downcase username))))))))

(define (user-manager-login um username password)
  (with-timing 'user-manager "user-manager-login"
    (define u (user-manager-lookup/username um username))
    (and u (password-valid? um u password) u)))

(define (user-manager-verify! um id verification-code)
  (with-timing 'user-manager "user-manager-verify!"
    (void
     (with-database-transaction [conn (user-manager-db um)]
       (query-exec conn (~> (from user #:as u)
                            (update [verified? #t])
                            (where (and (= u.id ,id)
                                        (= u.verification-code ,verification-code)))))))))

(define (user-manager-reset-password! um
                                      #:user-id id
                                      #:token token
                                      #:password password)
  (with-timing 'user-manager "user-manager-reset-password!"
    (with-database-transaction [conn (user-manager-db um)]
      (cond
        [(lookup-password-reset conn id token)
         => (lambda (_pr)
              (begin0 #t
                (clear-password-reset! conn id)
                (and~> (lookup conn
                               (~> (from user #:as u)
                                   (where (= u.id ,id))))
                       (set-password um _ password)
                       (update! conn _))))]


        [else #f]))))

(define (lookup-password-reset conn id token)
  (lookup conn (~> (from password-reset #:as pr)
                   (where (and (= pr.user-id ,id)
                               (= pr.token ,token)
                               (> pr.expires-at (now)))))))

(define (clear-password-reset! conn id)
  (query-exec conn (~> (from password-reset #:as pr)
                       (where (= pr.user-id ,id))
                       (delete))))
