#lang racket/base

(require (for-syntax racket/base)
         component
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
         "broker.rkt"
         "serialize.rkt")


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
     [("") (dashboard-page broker)]))

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

(define ((dashboard-page b) req)
  (page
   (haml
    (.container
     (.island
      (:h1.island__title "Jobs")
      (.island__content
       (job-list (broker-jobs b))))))))


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
      (:title "Koyo Jobs")
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
   (:ul.items
    ,@(for/list ([job (in-list jobs)])
        (haml
         (:li
          ([:class (class-list "item" (~a "item--" (hash-ref job 'status)))]
           [:data-job-uri (make-uri "jobs" (hash-ref job 'id))])
          (.item__queue
           (:span (hash-ref job 'queue)))
          (match (deserialize (hash-ref job 'arguments))
            [(list kws kw-args args)
             (haml
              (.item__arguments
               (:pre "(" (hash-ref job 'job))
               ,@(for/list ([kw (in-list kws)]
                            [kw-arg (in-list kw-args)])
                   (haml (:pre " " (~s kw) " " (~s kw-arg))))

               ,@(for/list ([arg (in-list args)])
                   (haml (:pre " " (~s arg))))
               (:pre " )")))])))))))

(define (class-list . classes)
  (string-join (filter values classes) " "))
