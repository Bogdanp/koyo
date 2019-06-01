#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         component
         crypto
         crypto/argon2
         db
         gregor
         koyo/database
         koyo/profiler
         openssl/md5
         racket/contract
         racket/function
         racket/port
         racket/random
         racket/string
         sql
         struct-plus-plus)

;; user ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out user) user++)

;; https://password-hashing.net/argon2-specs.pdf
(define ARGON2-CONFIG
  '((p 8)    ;; parallelism, adjust according to number of cores
    (t 512)  ;; iterations, adjust based on duration
    (m 4096) ;; memory per p in kb, adjust based on available memory
    ))

(struct++ user
  ([id maybe-id/c]
   [username non-empty-string? string-downcase]
   [(password-hash #f) (or/c false/c non-empty-string?)]
   [(verified? #f) boolean?]
   [(verification-code (generate-verification-code)) non-empty-string?]
   [(created-at (now/moment)) moment?]
   [(updated-at (now/moment)) moment?])
  #:transparent)

(define (generate-verification-code [strength 8192])
  (call-with-input-bytes (crypto-random-bytes strength) md5))

(define (set-user-password u p)
  (define password-hash
    (parameterize ([crypto-factories (list argon2-factory)])
      (pwhash 'argon2id (string->bytes/utf-8 p) ARGON2-CONFIG)))

  (set-user-password-hash u password-hash))

(define (user-password-valid? u p)
  (with-timing 'user "user-password-valid?"
    (parameterize ([crypto-factories (list argon2-factory)])
      (pwhash-verify #f (string->bytes/utf-8 p) (user-password-hash u)))))


;; user-manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 exn:fail:user-manager?
 exn:fail:user-manager:username-taken?

 user-manager
 user-manager?
 user-manager-lookup/id
 user-manager-lookup/username
 user-manager-create!
 user-manager-login
 user-manager-verify)

(struct exn:fail:user-manager exn:fail ())
(struct exn:fail:user-manager:username-taken exn:fail:user-manager ())

(struct++ user-manager ([db connection?])
  #:transparent
  #:methods gen:component
  [(define component-start identity)
   (define component-stop identity)])

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


(module+ test
  (require rackunit
           rackunit/text-ui
           (prefix-in config: "../config.rkt"))

  (define-system test
    [db (make-database-factory #:database config:test-db-name
                               #:username config:test-db-username
                               #:password config:test-db-password)]
    [user-manager (db) user-manager])

  (run-tests
   (test-suite
    "user-manager"
    #:before
    (lambda _
      (system-start test-system)
      (with-database-connection [conn (system-get test-system 'db)]
        (query-exec conn "truncate table users")))

    #:after
    (lambda _
      (system-stop test-system))

    (test-suite
     "user-manager-create!"

     (test-case "creates users"
       (check-match
        (user-manager-create! (system-get test-system 'user-manager) "bogdan" "hunter2")
        (user (? exact-positive-integer?) "bogdan" _ #f _ _ _))))

    (test-suite
     "user-manager-login"

     (test-case "returns #f when given invalid login data"
       (check-false (user-manager-login (system-get test-system 'user-manager) "invalid" "invalid"))
       (check-false (user-manager-login (system-get test-system 'user-manager) "bogdan" "invalid")))

     (test-case "returns a user upon successful login"
       (check-match
        (user-manager-login (system-get test-system 'user-manager) "bogdan" "hunter2")
        (user (? exact-positive-integer?) "bogdan" _ _ _ _ _)))))))
