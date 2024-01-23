#lang racket/base

(require net/url
         racket/contract/base
         racket/string
         web-server/http
         web-server/servlet/servlet-structs
         web-server/servlet/web
         "contract.rkt"
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
 (contract-out
  [current-continuation-key (parameter/c (or/c #f non-empty-string?))]
  [current-continuation-key-cookie-path (parameter/c non-empty-string?)]
  [current-continuation-key-cookie-secure? (parameter/c boolean?)]
  [current-continuation-mismatch-handler (parameter/c (-> request? response?))]
  [current-continuation-wrapper (parameter/c (-> (-> request? response?)
                                                 (-> request? response?)))]
  [protect-continuation (-> (-> request? response?)
                            (-> request? response?))]
  [wrap-protect-continuations middleware/c]

  [send/suspend/protect (-> (-> string? can-be-response?) request?)]
  [send/back/protect (-> response? void?)]
  [send/forward/protect (-> (-> string? can-be-response?) request?)]
  [send/suspend/dispatch/protect (-> (-> (-> (-> request? any) string?) can-be-response?) any)]
  [redirect/get/protect (->* [] [#:headers (listof header?)] request?)]
  [redirect/get/forget/protect (->* [] [#:headers (listof header?)] request?)]))

(define continuation-key-cookie-name "_k")

(define current-continuation-key
  (make-parameter #f))

(define current-continuation-key-cookie-path
  (make-parameter "/"))

(define current-continuation-key-cookie-secure?
  (make-parameter #t))

(define current-continuation-mismatch-handler
  (make-parameter
   (lambda (req)
     (redirect-to (url->string (url-scrub (request-uri req)))))))

(define current-continuation-wrapper
  (make-parameter values))

(define (find-continuation-key cookies)
  (for/first ([cookie (in-list cookies)]
              #:when (string=? continuation-key-cookie-name (client-cookie-name cookie)))
    (client-cookie-value cookie)))

(define (protect-request req)
  (with-timing 'continuation "protect-request"
    (define expected-key (current-continuation-key))
    (define maybe-key (find-continuation-key (request-cookies req)))
    (if (and maybe-key (string=? maybe-key expected-key))
        req
        (send/back ((current-continuation-mismatch-handler) req)))))

(define ((protect-continuation k) req)
  (define wrapped-k
    ((current-continuation-wrapper) k))
  (wrapped-k (protect-request req)))

(define ((wrap-protect-continuations handler) req . args)
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

    ;; By introducing a prompt here, we can ensure that even handlers
    ;; that escape (via send/suspend) return normally back into the
    ;; wrapper stack, assuming `wrap-protect-continuations' is placed
    ;; at the top of said stack.
    (define res
      (call-with-continuation-prompt
       (lambda ()
         ;; Extend the parameterization here (as opposed to outside of
         ;; the `call/prompt') to ensure the key is available to
         ;; `protect-request' later.
         ;;
         ;; xref. https://github.com/racket/racket/issues/4216
         (parameterize ([current-continuation-key continuation-key])
           (apply handler req args)))
       servlet-prompt))

    (struct-copy response res [headers (cons
                                        (cookie->header the-cookie)
                                        (response-headers res))])))

(define (send/suspend/protect f)
  (protect-request (send/suspend f)))

(define send/back/protect send/back)

(define (send/forward/protect f)
  (protect-request (send/forward f)))

(define (send/suspend/dispatch/protect f)
  (send/suspend/dispatch
   (lambda (embed/url)
     (f (compose1 embed/url protect-continuation)))))

(define (redirect/get/protect #:headers [hs null])
  (send/suspend/protect
   (lambda (k-url)
     (redirect-to k-url #:headers hs))))

(define (redirect/get/forget/protect #:headers [hs null])
  (send/forward/protect
   (lambda (k-url)
     (redirect-to k-url #:headers hs))))
