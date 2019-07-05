#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         component
         db
         gregor
         koyo/database
         koyo/profiler
         koyo/random
         racket/contract
         racket/function
         racket/string
         sql
         struct-plus-plus
         "hash.rkt")

;; user ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out user)
 (rename-out [user++ make-user]))

(struct++ user
  ([id maybe-id/c]
   [username non-empty-string? string-downcase]
   [(password-hash #f) (or/c false/c non-empty-string?)]
   [(verified? #f) boolean?]
   [(verification-code (generate-random-string)) non-empty-string?]
   [(created-at (now/moment)) moment?]
   [(updated-at (now/moment)) moment?])
  #:transparent)

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
  (define user (set-user-password (user++ #:id #f #:username username) password))
  (define id
    (with-handlers ([exn:fail:sql:constraint-violation?
                     (lambda _
                       (raise (exn:fail:user-manager:username-taken
                               (format "username '~a' is taken" username)
                               (current-continuation-marks))))])
      (with-database-transaction [conn (user-manager-db um)]
        (query-exec conn (insert #:into users
                                 #:set
                                 [username ,(user-username user)]
                                 [password_hash ,(user-password-hash user)]
                                 [verification_code ,(user-verification-code user)]
                                 [created_at ,(->sql-timestamp (user-created-at user))]
                                 [updated_at ,(->sql-timestamp (user-updated-at user))]))
        (query-value conn (select (lastval))))))

  (set-user-id user id))

(define-syntax (user-manager-lookup stx)
  (syntax-parse stx
    [(_ user-manager e ...)
     #'(with-database-connection [conn (user-manager-db user-manager)]
         (for/first ([(id username password-hash verified? verification-code created-at updated-at)
                      (in-row conn (select id username password_hash
                                           verified verification_code
                                           created_at updated_at
                                           #:from users e ...))])
           (user++ #:id id
                   #:username username
                   #:password-hash password-hash
                   #:verified? verified?
                   #:verification-code verification-code
                   #:created-at created-at
                   #:updated-at updated-at)))]))

(define/contract (user-manager-lookup/id um id)
  (-> user-manager? exact-positive-integer? (or/c false/c user?))
  (with-timing 'user-manager (format "(user-manager-lookup/id ~v)" id)
    (user-manager-lookup um #:where (= id ,id))))

(define/contract (user-manager-lookup/username um username)
  (-> user-manager? string? (or/c false/c user?))
  (with-timing 'user-manager (format "(user-manager-lookup/username ~v)" username)
    (user-manager-lookup um #:where (= username ,username))))

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
       (query-exec conn (update users
                                #:set [verified ,#t]
                                #:where (and (= id ,id)
                                             (= verification_code ,verification-code))))))))
