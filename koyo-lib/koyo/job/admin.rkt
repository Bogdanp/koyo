#lang at-exp racket/base

(require (for-syntax racket/base)
         gregor
         koyo/continuation
         koyo/http
         net/uri-codec
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/port
         racket/pretty
         racket/runtime-path
         racket/string
         web-server/dispatchers/dispatch
         web-server/servlet
         "../haml.rkt"
         "broker.rkt")


;; component ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-broker-admin-factory
 broker-admin?
 broker-admin-handler)

(struct broker-admin (handler)
  #:transparent)

(define/contract ((make-broker-admin-factory [path "/_koyo/jobs"]) broker)
  (->* () (string?) (-> broker? broker-admin?))
  (broker-admin (make-handler path broker)))

(define (make-handler path-prefix broker)
  (define prefix (string-split path-prefix "/"))
  (define prefix:len (length prefix))
  (lambda (req)
    (define path (map path/param-path (url-path (request-uri req))))
    (define subpath (drop path prefix:len))
    (parameterize ([current-broker broker]
                   [current-path (string-join path "/")]
                   [current-path-prefix path-prefix])
      (match subpath
        [(list)
         (dashboard-page req)]

        [(list "jobs" (app string->number id))
         (job-page req id)]

        [_
         (next-dispatcher)]))))


;; pages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path assets
  (build-path "assets"))

(define current-path-prefix
  (make-parameter #f))

(define current-path
  (make-parameter #f))

(define SCRIPT
  (call-with-input-file (build-path assets "app.js")
    port->string))

(define STYLE
  (call-with-input-file (build-path assets "screen.css")
    port->string))

(define (dashboard-page req)
  (define the-cursor
    (bindings-ref-number (request-bindings/raw req) 'cursor))
  (define the-jobs
    (broker-jobs (current-broker)
                 (or the-cursor -1)))
  (page
   (haml
    (.container
     (.island
      (:h1.island__title "Jobs")
      (.island__content
       (job-list the-jobs))
      (unless (null? the-jobs)
        (haml
         (.island__footer
          (button "Next Page" (make-uri #:params `((cursor . ,(number->string (job-meta-id (last the-jobs)))))))))))
     (:br)))))

(define (job-page _req id)
  (define j (broker-job (current-broker) id))
  (unless j (next-dispatcher))
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (page
      (haml
       (.container
        (.island
         (:h1.island__title @~a{Job @(job-meta-id j)})
         (.island__content
          (job-table j embed/url)))))))))


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-uri #:params [params null]
                  . parts)
  (define the-uri
    (~a (current-path-prefix) "/" (string-join (map ~a parts) "/")))
  (define trimmed-uri
    (string-trim the-uri "/"
                 #:left? #f
                 #:right? #t))
  (if (null? params)
      trimmed-uri
      (~a trimmed-uri "?" (alist->form-urlencoded params))))

(define (page . content)
  (define (nav-item label path)
    (define path*
      (make-uri path))

    (define classes
      (class-list "nav__item"
                  (and (string=? path* (current-path))
                       "nav__item--active")))

    (haml
     (:li
      ([:class classes])
      (:a ([:href path*]) label))))

  (response/xexpr
   (haml
    (:html
     (:head
      (:title "Jobs")
      (:style
       ([:type "text/css"])
       STYLE)
      (:script SCRIPT))
     (:body
      (:ul.nav
       (.container
        (nav-item "Dashboard" "")))
      (.content
       ,@content))))))

(define (job-list jobs)
  (haml
   (:table.jobs-table
    (:thead
     (:tr
      (:th "ID")
      (:th "Job")
      (:th "Status")
      (:th "Queue")
      (:th "Created")))
    (:tbody
     ,@(map job-list-item jobs)))))

(define (job-list-item j)
  (haml
   (:tr
    (:td (:pre (~a "#" (job-meta-id j))))
    (:td (pp-job j))
    (:td (pp-status (job-meta-status j)))
    (:td (:span.status (job-meta-queue j)))
    (:td (pp-moment (job-meta-created-at j))))))

(define (job-table j embed/url)
  (define (row label content)
    (haml
     (:tr
      (:th label)
      (:td content))))

  (haml
   (:table.job-table
    (row "Queue" (job-meta-queue j))
    (row "Job" (pp-job j))
    (row "Status" (pp-status (job-meta-status j)))
    (row "Attempts" (number->string (job-meta-attempts j)))
    (row "Created" (pp-moment (job-meta-created-at j)))
    (row "Scheduled" (pp-moment (job-meta-scheduled-at j)))
    (row "Started" (cond
                     [(job-meta-started-at j) => pp-moment]
                     [else 'mdash]))
    (row "Actions" (haml
                    (:div
                     (button
                      #:confirmation-required? #t
                      #:style 'primary
                      "Retry"
                      (embed/url
                       (lambda (_req)
                         (broker-mark-for-retry! (current-broker) (job-meta-id j) (now/moment))
                         (redirect/get/forget)
                         (redirect-to (make-uri "jobs" (job-meta-id j))))))
                     " "
                     (button
                      #:confirmation-required? #t
                      #:style 'danger
                      "Delete"
                      (embed/url
                       (lambda (_req)
                         (broker-delete! (current-broker) (job-meta-id j))
                         (redirect/get/forget)
                         (redirect-to (make-uri )))))))))))

(define (button label action
                #:style [style #f]
                #:confirmation-required? [confirmation-required? #f])
  (haml
   (:a
    ([:class (class-list "button" (and style (~a "button--" style)))]
     [:href action]
     [:onclick (if confirmation-required?
                   "return confirm('Are you sure?')"
                   "")])
    label)))

(define (pp-job j)
  (match-define (list kws kw-args args)
    (job-meta-arguments j))
  (define expr-str
    (call-with-output-string
     (lambda (out)
       (parameterize ([current-output-port out]
                      [pretty-print-columns 40])
         (pretty-write `(,(string->symbol (job-meta-job j))
                         ,@(flatten
                            (for/list ([kw (in-list kws)]
                                       [kw-arg (in-list kw-args)])
                              (list kw kw-arg)))
                         ,@args))))))

  (haml
   (:a
    ([:href (make-uri "jobs" (job-meta-id j))])
    (:pre expr-str))))

(define (pp-status s)
  (haml
   (:span
    ([:class (class-list "status" (~a "status--" s))])
    s)))

(define (pp-moment m)
  (define now/posix  (->posix (now/moment)))
  (define when/posix (->posix m))
  (define delta
    (inexact->exact
     (truncate
      (exact->inexact
       (abs (- now/posix when/posix))))))

  (define second 1)
  (define minute (* 60 second))
  (define hour   (* 60 minute))
  (define day    (* 24 hour))
  (define week   (* 7  day))

  (define period
    (cond
      [(>  delta (* 2 week))   @~a{@(quotient delta week) weeks}]
      [(>= delta      week)    @~a{1 week}]
      [(>  delta (* 2 day))    @~a{@(quotient delta day) days}]
      [(>= delta      day)     @~a{1 day}]
      [(>  delta (* 2 hour))   @~a{@(quotient delta hour) hours}]
      [(>= delta      hour)    @~a{1 hour}]
      [(>= delta (* 2 minute)) @~a{@(quotient delta minute) minutes}]
      [(>= delta      minute)  @~a{1 minute}]
      [(=  delta      second)  @~a{1 second}]
      [else                    @~a{@delta seconds}]))

  (cond
    [(= now/posix when/posix) @~a{now}]
    [(> now/posix when/posix) @~a{@period ago}]
    [else                     @~a{in @period}]))

(define (class-list . classes)
  (string-join (filter values classes) " "))
