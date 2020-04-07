#lang at-exp racket/base

(require (for-syntax racket/base)
         component
         gregor
         net/url
         racket/contract
         racket/format
         racket/match
         racket/port
         racket/runtime-path
         racket/string
         web-server/dispatch
         web-server/dispatchers/dispatch
         web-server/http
         web-server/servlet-dispatch
         "../haml.rkt"
         "broker.rkt")


;; component ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-broker-admin-factory
 broker-admin?
 broker-admin-dispatcher)

(struct broker-admin (dispatcher)
  #:transparent
  #:methods gen:component
  [(define component-start values)
   (define component-stop values)])

(define/contract ((make-broker-admin-factory [path "/_koyo/jobs"]) broker)
  (->* () (non-empty-string?) (-> broker? broker-admin?))
  (broker-admin (make-dispatcher path broker)))


;; dispatcher ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-dispatcher path broker)
  (current-path-prefix path)

  (define-values (app _)
    (dispatch-rules
     [("") (dashboard-page broker)]
     [("jobs" (integer-arg)) (job-page broker)]))

  (mount path (dispatch/servlet app)))

(define ((mount p dispatcher) conn req)
  (cond
    [(request-mount req p)
     => (lambda (mounted-req)
          (current-path (string-join (map path/param-path (url-path (request-uri req))) "/"))
          (dispatcher conn mounted-req))]

    [else
     (next-dispatcher)]))

(define (request-mount req p)
  (define uri
    (request-uri req))

  (let loop ([path (url-path uri)]
             [parts (string-split p "/")])
    (cond
      [(null? parts)
       (define uri*
         (struct-copy url uri [path (if (null? path)
                                        (list (path/param "" null))
                                        path)]))

       (struct-copy request req [uri uri*])]

      [(null? path) #f]

      [(string=? (path/param-path (car path)) (car parts))
       (loop (cdr path) (cdr parts))]

      [else #f])))


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

(define ((dashboard-page b) _req)
  (page
   (haml
    (.container
     (.island
      (:h1.island__title "Jobs")
      (.island__content
       (job-list (broker-jobs b))))))))

(define ((job-page b) _req id)
  (cond
    [(broker-job b id)
     => (lambda (j)
          (page
           (haml
            (.container
             (.island
              (:h1.island__title @~a{Job @(job-meta-id j)})
              (.island__content
               (job-table j)))))))]

    [else
     (next-dispatcher)]))


;; widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-uri . parts)
  (define the-uri
    (~a (current-path-prefix) "/" (string-join (map ~a parts) "/")))

  (string-trim the-uri "/"
               #:left? #f
               #:right? #t))

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
  (haml (:ul.items ,@(map job-list-item jobs))))

(define (job-list-item j)
  (haml
   (:li
    ([:class (class-list "item" @~a{item--@(job-meta-status j)})]
     [:data-job-uri (make-uri "jobs" (job-meta-id j))])
    (.item__queue
     (:span (job-meta-queue j)))
    (.item__arguments
     ,@(pp-job j)))))

(define (job-table j)
  (define (row label content)
    (haml
     (:tr
      (:th label)
      (:td content))))

  (haml
   (:table.job-table
    (row "Queue" (job-meta-queue j))
    (row "Job" (haml (:div ,@(pp-job j))))
    (row "Status" (pp-status (job-meta-status j)))
    (row "Created" (pp-moment (job-meta-created-at j)))
    (row "Scheduled" (pp-moment (job-meta-scheduled-at j)))
    (row "Started" (cond
                     [(job-meta-started-at j) => pp-moment]
                     [else 'mdash])))))

(define (pp-job j)
  (match-define (list kws kw-args args)
    (job-meta-arguments j))

  (haml
   (:pre "(" (job-meta-job j))

   (:div
    ,@(for/list ([kw (in-list kws)]
                 [kw-arg (in-list kw-args)])
        (haml (:pre " " (~s kw) " " (~s kw-arg)))))

   (:div
    ,@(for/list ([arg (in-list args)])
        (haml (:pre " " (~s arg)))))

   (:pre " )")))

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
