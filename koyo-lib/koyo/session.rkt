#lang racket/base

(require component
         racket/contract
         racket/file
         racket/format
         racket/function
         racket/generic
         racket/serialize
         racket/string
         web-server/http
         web-server/http/id-cookie
         "profiler.rkt"
         "random.rkt"
         "util.rkt")

;; Session stores ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 gen:session-store
 session-store?
 session-store-generate-id!
 session-store-load!
 session-store-persist!
 session-store-ref
 session-store-set!
 session-store-update!
 session-store-remove!

 memory-session-store?
 make-memory-session-store)

(define-generics session-store
  (session-store-generate-id! session-store)
  (session-store-load! session-store)
  (session-store-persist! session-store)
  (session-store-ref session-store session-id key default)
  (session-store-set! session-store session-id key value)
  (session-store-update! session-store session-id key f default)
  (session-store-remove! session-store session-id key))

;; seq: a monotonically-increasing sequence number that ensures session ids cannot collide
;; sessions: a hash from session ids to pairs of timestamps and data hashes
(serializable-struct mss-data (seq sessions)
  #:transparent)

(define-logger memory-session-store)

(struct memory-session-store (custodian data file-path)
  #:methods gen:session-store
  [(define (session-store-generate-id! ss)
     (~a (memory-session-store-next-id! ss) "." (generate-random-string)))

   (define (session-store-load! ss)
     (when (file-exists? (memory-session-store-file-path ss))
       (with-input-from-file (memory-session-store-file-path ss)
         (lambda ()
           (define data (read))
           (unless (eof-object? data)
             (set-box! (memory-session-store-data ss)
                       (deserialize data)))))))

   (define (session-store-persist! ss)
     (with-output-to-file (memory-session-store-file-path ss)
       #:exists 'truncate/replace
       (lambda ()
         (write (serialize (unbox (memory-session-store-data ss)))))))

   (define (session-store-ref ss session-id key default)
     (let* ([data (unbox (memory-session-store-data ss))]
            [sessions (mss-data-sessions data)]
            [session-pair (hash-ref sessions
                                    (string->symbol session-id)
                                    (cons (current-seconds) (hasheq)))]
            [session-data (cdr session-pair)])
       (hash-ref session-data key default)))

   (define (session-store-set! ss session-id key value)
     (memory-session-store-update-session! ss session-id (curryr hash-set key value)))

   (define (session-store-update! ss session-id key value default)
     (memory-session-store-update-session! ss session-id (curryr hash-update key value default)))

   (define (session-store-remove! ss session-id key)
     (memory-session-store-update-session! ss session-id (curryr hash-remove key)))])

(define (memory-session-store-next-id! ss)
  (let ([next-id #f])
    (box-swap!
     (memory-session-store-data ss)
     (lambda (data)
       (set! next-id (add1 (mss-data-seq data)))
       (struct-copy mss-data data [seq next-id])))

    next-id))

(define (memory-session-store-update-session! ss session-id f)
  (box-swap!
   (memory-session-store-data ss)
   (lambda (data)
     (define sessions
       (hash-update (mss-data-sessions data)
                    (string->symbol session-id)
                    (lambda (session-data)
                      (cons (current-seconds) (f (cdr session-data))))
                    (cons (current-seconds) (hasheq))))

     (struct-copy mss-data data [sessions sessions]))))

(define ((memory-session-store-remove-stale-sessions! ttl) data)
  (define live-sessions
    (for/fold ([live-sessions (hasheq)])
              ([(session-id session-data) (in-hash (mss-data-sessions data))])
      (cond
        [(< (current-seconds) (+ (car session-data) ttl))
         (hash-set live-sessions session-id session-data)]

        [else
         (log-memory-session-store-debug "session ~v expired" session-id)
         live-sessions])))

  (struct-copy mss-data data [sessions live-sessions]))

(define/contract (make-memory-session-store #:ttl [ttl (* 7 86400)]
                                            #:file-path [file-path (make-temporary-file)])
  (->* () (#:ttl exact-positive-integer?
           #:file-path path-string?)
       session-store?)

  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define data-box (box (mss-data 0 (hasheq))))
    (thread
     (lambda ()
       (let loop ()
         (sleep ttl)
         (log-memory-session-store-debug "expiring stale sessions")
         (box-swap! data-box (memory-session-store-remove-stale-sessions! ttl))
         (loop))))

    (memory-session-store custodian data-box file-path)))


;; Session manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-session-id
 session-manager?
 make-session-manager-factory
 session-manager-ref
 session-manager-set!
 session-manager-update!
 session-manager-remove!)

(define-logger session)

(define/contract current-session-id
  (parameter/c (or/c false/c non-empty-string?))
  (make-parameter #f))

(struct session-manager (cookie-name
                         cookie-path
                         cookie-secure?
                         cookie-http-only?
                         cookie-same-site
                         shelf-life
                         secret-key
                         store)
  #:methods gen:component
  [(define (component-start sm)
     (begin0 sm
       (session-store-load! (session-manager-store sm))))

   (define (component-stop sm)
     (begin0 sm
       (session-store-persist! (session-manager-store sm))))])

(define/contract ((make-session-manager-factory #:cookie-name cookie-name
                                                #:cookie-path [cookie-path "/"]
                                                #:cookie-secure? [cookie-secure? #t]
                                                #:cookie-http-only? [cookie-http-only? #t]
                                                #:cookie-same-site [cookie-same-site 'strict]
                                                #:shelf-life shelf-life
                                                #:secret-key secret-key
                                                #:store store))
  (->* (#:cookie-name non-empty-string?
        #:shelf-life exact-positive-integer?
        #:secret-key bytes?
        #:store session-store?)
       (#:cookie-path path-string?
        #:cookie-secure? boolean?
        #:cookie-http-only? boolean?
        #:cookie-same-site (or/c 'lax 'strict))
       (-> session-manager?))
  (session-manager cookie-name
                   cookie-path
                   cookie-secure?
                   cookie-http-only?
                   cookie-same-site
                   shelf-life
                   secret-key
                   store))

(define/contract session-manager-ref
  (case-> (-> session-manager? symbol? any/c)
          (-> session-manager? symbol? any/c any/c))

  (case-lambda
    [(sm key)
     (session-manager-ref sm key (lambda ()
                                   (raise-user-error 'session-manager-ref "no value found for key ~a" key)))]

    [(sm key default)
     (with-timing 'session "session-manager-ref"
       (session-store-ref (session-manager-store sm) (current-session-id) key default))]))

(define/contract (session-manager-set! sm key value)
  (-> session-manager? symbol? serializable? void?)
  (with-timing 'session "session-manager-set!"
    (session-store-set! (session-manager-store sm) (current-session-id) key value)))

(define/contract (session-manager-remove! sm key)
  (-> session-manager? symbol? void?)
  (with-timing 'session "session-manager-remove!"
    (session-store-remove! (session-manager-store sm) (current-session-id) key)))

(define/contract session-manager-update!
  (case-> (-> session-manager? symbol? (-> any/c serializable?) serializable?)
          (-> session-manager? symbol? (-> any/c serializable?) any/c serializable?))
  (case-lambda
    [(sm key f)
     (session-manager-update! sm key f (lambda ()
                                         (raise-user-error 'session-manager-update! "no value found for key ~a" key)))]

    [(sm key f default)
     (with-timing 'session "session-manager-update!"
       (session-store-update! (session-manager-store sm) (current-session-id) key f default))]))

(define (session-manager-cookie-extension sm)
  (format "SameSite=~a" (case (session-manager-cookie-same-site sm)
                          [(lax) "Lax"]
                          [(strict) "Strict"])))

(define (session-manager-cookie sm session-id)
  (make-id-cookie (session-manager-cookie-name sm) session-id
                  #:path (session-manager-cookie-path sm)
                  #:secure? (session-manager-cookie-secure? sm)
                  #:http-only? (session-manager-cookie-http-only? sm)
                  #:extension (session-manager-cookie-extension sm)
                  #:key (session-manager-secret-key sm)
                  #:max-age (session-manager-shelf-life sm)))


;; Middleware ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 wrap-session)

(define/contract (((wrap-session sm) handler) req)
  (-> session-manager? (-> (-> request? response?)
                           (-> request? response?)))
  (with-timing 'session "wrap-session"
    (define store (session-manager-store sm))
    (define session-id
      (or (request-id-cookie req
                             #:name (session-manager-cookie-name sm)
                             #:key (session-manager-secret-key sm)
                             #:shelf-life (session-manager-shelf-life sm))
          (session-store-generate-id! store)))

    (parameterize ([current-session-id session-id])
      (define resp (handler req))
      (define headers
        (cons (cookie->header (session-manager-cookie sm session-id))
              (response-headers resp)))

      (struct-copy response resp [headers headers]))))
