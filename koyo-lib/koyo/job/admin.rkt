#lang at-exp racket/base

(require (for-syntax racket/base)
         (only-in db sql-null)
         gregor
         racket/contract/base
         racket/match
         racket/port
         racket/pretty
         racket/runtime-path
         racket/string
         struct-define
         web-server/servlet
         "../dispatch.rkt"
         "../guard.rkt"
         "../haml.rkt"
         "../http.rkt"
         "../mime.rkt"
         "../url.rkt"
         "broker.rkt")


;; dispatcher ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 broker-admin/c
 (contract-out
  [make-broker-admin (-> broker? (-> request? response?))]))

(define broker-admin/c
  (-> request? response?))

(define (make-broker-admin broker)
  (make-handler broker))


;; component (DEPRECATED) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 broker-admin?
 broker-admin-handler
 (contract-out
  [make-broker-admin-factory
   (->* []
        [string?]
        (-> broker? broker-admin?))]))

(struct broker-admin (handler)
  #:transparent)

(define ((make-broker-admin-factory [path "/_koyo/jobs"]) broker)
  (broker-admin
   (let ([root (string->url path)]
         [handler (make-handler broker)])
     (lambda (req)
       (parameterize ([current-reverse-uri-path-adjuster
                       (let ([old (current-reverse-uri-path-adjuster)])
                         (lambda (reverse-uri)
                           (old (string-append path reverse-uri))))])
         (handler (request-reroot req root)))))))


;; handler & pages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-handler broker)
  (define-values (dispatch reverse-uri _req-roles)
    (dispatch-rules+roles
     [("")
      dashboard-page]
     [("api" "v1" "jobs")
      (jobs-page broker)]
     [("api" "v1" "jobs" (integer-arg))
      (job-page broker)]
     [("api" "v1" "jobs" (integer-arg) "retry")
      #:method "post"
      (retry-job-page broker)]
     [("api" "v1" "jobs" (integer-arg))
      #:method "delete"
      (delete-job-page broker)]
     [("api" "v1" "queues")
      (queues-page broker)]
     [("api" "v1" "workers")
      (workers-page broker)]
     [("static" (string-arg))
      #:name 'static-page
      static-page]
     [else
      dashboard-page]))
  (lambda (req)
    (parameterize ([current-broker broker]
                   [current-reverse-uri-fn reverse-uri])
      (dispatch req))))

(define (dashboard-page _req)
  (response/xexpr
   #:preamble #"<!doctype html>"
   (haml
    (:html
     ([:lang "en-US"])
     (:head
      (:title "Koyo Jobs")
      (:meta
       ([:charset "utf-8"]))
      (:meta
       ([:content "width=device-width, initial-scale=1"]
        [:name "viewport"]))
      (:link
       ([:href (reverse-uri 'static-page "app.css")]
        [:rel "stylesheet"]
        [:type "text/css"]))
      (:script
       ([:defer "defer"]
        [:src (reverse-uri 'static-page "app.js")])))
     (:body
      (:div#app
       ([:data-root
         (string-trim
          #:left? #f
          #;str (reverse-uri 'dashboard-page)
          #;sep "/")])))))))

(define-runtime-path assets
  (build-path "assets"))

(define get-asset
  (let ([memo (make-hash)])
    (lambda (filename)
      (hash-ref!
       #;ht memo
       #;key filename
       #;fail-proc
       (lambda ()
         (call-with-input-file (build-path assets filename)
           port->bytes))))))

(define (static-page _req filename)
  (let ([filename (path-add-extension filename #".gz" #".")])
    (with-guard (λ () (response/empty))
      (guard (member filename (directory-list assets)))
      (define data
        (get-asset filename))
      (response/full
       #;code 200
       #;message #"OK"
       #;seconds (current-seconds)
       #;mime (path->mime-type filename)
       #;headers (list
                  (make-header #"content-encoding" #"gzip")
                  (make-header #"content-length" (string->bytes/utf-8 (number->string (bytes-length data)))))
       #;body (list data)))))

(define ((jobs-page broker) req)
  (define binds (request-bindings/raw req))
  (define queue (or (bindings-ref binds 'queue) sql-null))
  (define cursor (or (bindings-ref-number binds 'cursor) -1))
  (define statuses
    (let ([status (bindings-ref binds 'status)])
      (or (and status (string-split status ",")) sql-null)))
  (response/jsexpr
   (->jsexpr
    (broker-jobs broker queue cursor statuses))))

(define ((job-page broker) _req id)
  (with-guard (λ () (response/jsexpr
                     #:code 404
                     (hasheq 'error "job not found")))
    (response/jsexpr
     (->jsexpr
      (guard (broker-job broker id))))))

(define ((retry-job-page broker) _req id)
  (broker-mark-for-retry! broker id (now/moment))
  (response/empty))

(define ((delete-job-page broker) _req id)
  (broker-delete! broker id)
  (response/empty))

(define ((queues-page broker) _req)
  (response/jsexpr
   (->jsexpr
    (broker-queues broker))))

(define ((workers-page broker) _req)
  (response/jsexpr
   (->jsexpr
    (broker-workers broker))))

(define (->jsexpr v)
  (cond
    [(worker-meta? v)
     (worker-meta->jsexpr v)]
    [(queue-meta? v)
     (queue-meta->jsexpr v)]
    [(job-meta? v)
     (job-meta->jsexpr v)]
    [(moment? v)
     (moment->iso8601 v)]
    [(symbol? v)
     (symbol->string v)]
    [(hash? v)
     (for/hasheq ([(k v) (in-hash v)])
       (values k (->jsexpr v)))]
    [(list? v)
     (for/list ([v (in-list v)])
       (->jsexpr v))]
    [(vector? v)
     (for/list ([v (in-vector v)])
       (->jsexpr v))]
    [else v]))

(define (worker-meta->jsexpr v)
  (struct-define worker-meta v)
  (hasheq
   'id id
   'pid pid
   'hostname hostname
   'heartbeat (->jsexpr heartbeat)
   'up-since (->jsexpr up-since)))

(define (queue-meta->jsexpr v)
  (struct-define queue-meta v)
  (hasheq
   'id id
   'total-ready total-ready
   'total-running total-running
   'total-done total-done
   'total-failed total-failed))

(define (job-meta->jsexpr v)
  (struct-define job-meta v)
  (hasheq
   'id id
   'queue queue
   'name job
   'arguments (~arguments arguments)
   'status (->jsexpr status)
   'priority priority
   'attempts attempts
   'created-at (->jsexpr created-at)
   'scheduled-at (->jsexpr scheduled-at)
   'started-at (->jsexpr started-at)
   'worker-id worker-id))

(define (~arguments arguments)
  (match-define (list kws kw-args args)
    arguments)
  (append
   (for/list ([kw (in-list kws)]
              [kw-arg (in-list kw-args)])
     (define arg-str (pretty-format kw-arg))
     (~trimmed (format "~s ~a" kw arg-str)))
   (for/list ([arg (in-list args)])
     (~trimmed (pretty-format arg)))))

(define (~trimmed s)
  (if (> (string-length s) 79)
      (string-append (substring s 0 79) "…")
      s))
