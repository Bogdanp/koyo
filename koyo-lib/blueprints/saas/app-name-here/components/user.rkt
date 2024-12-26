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
         racket/generic
         racket/string
         struct-define
         threading
         "json.rkt")

;; user ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 role/c
 (schema-out user)
 (contract-out
  [user-has-roles? (-> user? (listof symbol?) boolean?)]))

(define role/c
  (or/c 'user 'admin))

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [(roles #(user)) (array/f symbol/f) #:contract (vectorof role/c)]
   [username string/f #:contract non-empty-string? #:wrapper string-downcase]
   [(password-hash "") string/f]
   [(verified? #f) boolean/f]
   [(verification-code (generate-random-string)) string/f #:contract non-empty-string?]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f])
  #:pre-persist-hook
  (lambda (u)
    (set-user-updated-at u (now/moment)))
  #:methods gen:to-jsexpr
  [(define/generic super ->jsexpr)
   (define (->jsexpr u)
     (struct-define user u)
     (hasheq
      'id id
      'roles (super roles)
      'username username
      'created-at (super created-at)
      'updated-at (super updated-at)))])

(define (user-has-roles? u roles)
  (null? (for/fold ([remaining roles])
                   ([role (in-vector (user-roles u))])
           (remq role remaining))))

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
  [make-user-manager
   (-> database? hasher? user-manager?)]
  [user-manager?
   (-> any/c boolean?)]
  [create-user!
   (-> user-manager? string? string? user?)]
  [create-password-reset-token!
   (-> user-manager?
       #:username non-empty-string?
       #:ip-address non-empty-string?
       #:user-agent non-empty-string?
       (values (or/c #f user?)
               (or/c #f string?)))]
  [lookup-user-by-id
   (-> user-manager? exact-positive-integer? (or/c #f user?))]
  [lookup-user-by-username
   (-> user-manager? string? (or/c #f user?))]
  [login
   (-> user-manager? string? string? (or/c #f user?))]
  [verify-user!
   (-> user-manager? exact-positive-integer? string? void?)]
  [reset-user-password!
   (-> user-manager?
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

(define (create-user! um username password)
  (define the-user
    (~> (make-user #:username username)
        (set-password um _ password)))
  (with-handlers ([exn:fail:sql:constraint-violation?
                   (lambda (_e)
                     (raise (exn:fail:user-manager:username-taken
                             (format "username '~a' is taken" username)
                             (current-continuation-marks))))])
    (with-database-connection [conn (user-manager-db um)]
      (insert-one! conn the-user))))

(define (create-password-reset-token!
         #:username username
         #:ip-address ip-address
         #:user-agent user-agent
         um)
  (with-timing 'user-manager "create-password-reset-token!"
    (with-database-transaction [conn (user-manager-db um)]
      (cond
        [(lookup-user-by-username um username)
         => (lambda (u)
              (~> (from password-reset #:as pr)
                  (where (= pr.user-id ,(user-id u)))
                  (delete)
                  (query-exec conn _))
              (values u (~> (make-password-reset
                             #:user-id (user-id u)
                             #:ip-address ip-address
                             #:user-agent user-agent)
                            (insert-one! conn _)
                            (password-reset-token))))]

        [else
         (values #f #f)]))))

(define (lookup-user-by-id um id)
  (with-timing 'user-manager (format "(lookup-user-by-id ~v)" id)
    (with-database-connection [conn (user-manager-db um)]
      (~> (from user #:as u)
          (where (= u.id ,id))
          (lookup conn _)))))

(define (lookup-user-by-username um username)
  (with-timing 'user-manager (format "(lookup-user-by-username ~v)" username)
    (with-database-connection [conn (user-manager-db um)]
      (~> (from user #:as u)
          (where (= u.username ,(string-downcase username)))
          (lookup conn _)))))

(define (login um username password)
  (with-timing 'user-manager "login"
    (define u (lookup-user-by-username um username))
    (and u (password-valid? um u password) u)))

(define (verify-user! um id verification-code)
  (with-timing 'user-manager "verify-user!"
    (with-database-transaction [conn (user-manager-db um)]
      (~> (from user #:as u)
          (update [verified? #t])
          (where (and (= u.id ,id)
                      (= u.verification-code ,verification-code)))
          (query-exec conn _)))))

(define (reset-user-password!
         #:user-id id
         #:token token
         #:password password
         um)
  (with-timing 'user-manager "reset-user-password!"
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
  (~> (from password-reset #:as pr)
      (where (and (= pr.user-id ,id)
                  (= pr.token ,token)
                  (> pr.expires-at (now))))
      (lookup conn _)))

(define (clear-password-reset! conn id)
  (~> (from password-reset #:as pr)
      (where (= pr.user-id ,id))
      (delete)
      (query-exec conn _)))
