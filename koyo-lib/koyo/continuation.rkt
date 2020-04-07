#lang racket/base

(require net/url
         racket/contract
         racket/function
         racket/string
         web-server/http
         web-server/servlet/servlet-structs
         web-server/servlet/web
         "http.rkt"
         "profiler.rkt"
         "random.rkt")

;; The main advantage to using continuations in a web context is that
;; they let you avoid the inversion of control problem.  However, that
;; comes at a price.  If someone guesses the URL of one of a user's
;; continuations then that person can essentially take over their
;; session.
;;
;; The decorators exposed by this module help prevent those sorts of
;; issues by pinning each continuation to a single web browser
;; session.

(provide
 current-continuation-key
 current-continuation-key-cookie-path
 current-continuation-key-cookie-secure?
 current-continuation-mismatch-handler
 current-continuation-wrapper
 protect-continuation
 wrap-protect-continuations

 send/suspend/protect
 send/forward/protect
 send/suspend/dispatch/protect
 redirect/get/protect
 redirect/get/forget/protect)

(define continuation-key-cookie-name "_k")

(define/contract current-continuation-key
  (parameter/c (or/c false/c non-empty-string?))
  (make-parameter #f))

(define/contract current-continuation-key-cookie-path
  (parameter/c non-empty-string?)
  (make-parameter "/"))

(define/contract current-continuation-key-cookie-secure?
  (parameter/c boolean?)
  (make-parameter #t))

(define/contract current-continuation-mismatch-handler
  (parameter/c (-> request? response?))
  (make-parameter
   (lambda (req)
     (redirect-to (url->string (url-scrub (request-uri req)))))))

(define/contract current-continuation-wrapper
  (parameter/c (-> (-> request? response?)
                   (-> request? response?)))
  (make-parameter values))

(define continuation-key-cookie?
  (compose1 (curry string=? continuation-key-cookie-name) client-cookie-name))

(define (find-continuation-key cookies)
  (define cookie
    (findf continuation-key-cookie? cookies))
  (and cookie (client-cookie-value cookie)))

(define/contract (protect-request req)
  (-> request? request?)
  (with-timing 'continuation "protect-request"
    (cond
      [(equal? (find-continuation-key (request-cookies req))
               (current-continuation-key))
       req]

      [else
       (send/back ((current-continuation-mismatch-handler) req))])))

(define/contract ((protect-continuation k) req)
  (-> (-> request? can-be-response?)
      (-> request? can-be-response?))
  (define wrapped-k
    ((current-continuation-wrapper) k))

  (wrapped-k (protect-request req)))

(define/contract ((wrap-protect-continuations handler) req)
  (-> (-> request? can-be-response?)
      (-> request? can-be-response?))
  (with-timing 'continuation "wrap-protect-continuations"
    (define continuation-key
      (or (find-continuation-key (request-cookies req))
          (generate-random-string)))

    (define the-cookie
      (make-cookie #:path (current-continuation-key-cookie-path)
                   #:secure? (current-continuation-key-cookie-secure?)
                   #:http-only? #t
                   #:extension "SameSite=Strict"
                   continuation-key-cookie-name
                   continuation-key))

    (define res
      (call-with-continuation-prompt
       (lambda ()
         (parameterize ([current-continuation-key continuation-key])
           (handler req)))
       servlet-prompt))

    (struct-copy response res [headers (cons
                                        (cookie->header the-cookie)
                                        (response-headers res))])))

(define/contract (send/suspend/protect f)
  (-> (-> string? can-be-response?) request?)
  (protect-request (send/suspend f)))

(define/contract (send/forward/protect f)
  (-> (-> string? can-be-response?) request?)
  (protect-request (send/forward f)))

(define/contract (send/suspend/dispatch/protect f)
  (-> (-> (-> (-> request? any) string?) can-be-response?) any)
  (send/suspend/dispatch
   (lambda (embed/url)
     (f (compose1 embed/url protect-continuation)))))

(define/contract (redirect/get/protect #:headers [hs null])
  (->* () (#:headers (listof header?)) request?)
  (send/suspend/protect
   (lambda (k-url)
     (redirect-to k-url #:headers hs))))

(define/contract (redirect/get/forget/protect #:headers [hs null])
  (->* () (#:headers (listof header?)) request?)
  (send/forward/protect
   (lambda (k-url)
     (redirect-to k-url #:headers hs))))
