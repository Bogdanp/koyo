#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/format
         web-server/servlet
         xml
         "haml.rkt"
         "util.rkt")


;; Timings and profiles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 profiler-enabled?

 record-timing
 with-timing

 timing?
 timing-id
 timing-parent
 timing-label
 timing-description
 timing-duration

 current-profile
 make-profile
 profile?
 profile-timings
 profile-write

 wrap-profiler)

(define/contract profiler-enabled?
  (parameter/c boolean?)
  (make-parameter #f))

(struct timing (id parent label description duration))
(struct pdata (seq timings))
(struct profile (data))

(define/contract (make-profile)
  (-> profile?)
  (profile (box (pdata 0 null))))

(define/contract profile-timings
  (-> profile? (listof timing?))
  (compose1 pdata-timings unbox profile-data))

(define (profile-generate-id! profile)
  (let ([next-id #f])
    (box-swap!
     (profile-data profile)
     (lambda (data)
       (set! next-id (add1 (pdata-seq data)))
       (struct-copy pdata data [seq next-id])))
    next-id))

(define (profile-add-timing! profile timing)
  (box-swap!
   (profile-data profile)
   (lambda (data)
     (define timings (cons timing (pdata-timings data)))
     (struct-copy pdata data [timings timings]))))

(define (profile-find-roots profile)
  (reverse
   (filter (lambda (t)
             (= (timing-parent t) 0))
           (profile-timings profile))))

(define (profile-find-children profile timing)
  (reverse
   (filter (lambda (t)
             (= (timing-id timing) (timing-parent t)))
           (profile-timings profile))))

(define/contract current-profile
  (parameter/c profile?)
  (make-parameter (make-profile)))

(define current-profile-label
  (make-parameter 'root))

(define current-timing-id
  (make-parameter 0))

(define (record-timing #:profile [profile (current-profile)]
                       #:label [label (current-profile-label)]
                       #:description description
                       f)
  (define p  #f)
  (define id #f)
  (define st #f)
  (dynamic-wind
    (lambda ()
      (set! p  (current-timing-id))
      (set! id (profile-generate-id! profile))
      (set! st (current-inexact-milliseconds))
      (current-timing-id id))
    (lambda ()
      (parameterize ([current-profile-label label])
        (f)))
    (lambda ()
      (define duration (- (current-inexact-milliseconds) st))
      (define current-timing (timing id p label description duration))
      (profile-add-timing! profile current-timing)
      (current-timing-id p))))

(define-syntax (with-timing stx)
  (syntax-parse stx
    [(_ (~optional label:expr #:defaults ([label #'(current-profile-label)]))
        description
        e ...+)
     #'(let ([f (lambda () e ...)])
         (cond
           [(profiler-enabled?)
            (record-timing #:label label
                           #:description description f)]

           [else (f)]))]))

(define/contract (profile-write [profile (current-profile)]
                                [out (current-output-port)])
  (->* () (profile? output-port?) void?)

  (define roots
    (profile-find-roots profile))

  (define (format-duration duration)
    (~a (~r duration #:precision '(= 2)) "ms"))

  (define (render-timing timing)
    (define toggle-id (format "uprofiler-timing-toggle-~a" (timing-id timing)))
    (haml
     (.uprofiler-timing
      (:input.uprofiler-timing-toggle
       ([:id toggle-id]
        [:type "checkbox"]))
      (:label
       ([:for toggle-id])
       (:span.uprofiler-timing-label
        (symbol->string (timing-label timing)))
       (:span.uprofiler-timing-description
        (:code (timing-description timing)))
       (:span.uprofiler-timing-duration
        (format-duration (timing-duration timing))))
      (@ (map render-timing (profile-find-children profile timing))))))

  (unless (null? roots)
    (define content
      (haml
       (.uprofiler-content
        (:input.uprofiler-toggle#uprofiler-toggle
         ([:type "checkbox"]))
        (:label.uprofiler-label
         ([:for "uprofiler-toggle"])
         (format-duration (apply + (map timing-duration roots))))
        (.uprofiler-timings
         (.uprofiler-timings-inner
          (@ (map render-timing roots)))
         (:label.uprofiler-close
          ([:for "uprofiler-toggle"])
          "Close")))))

    (write-xml/content (xexpr->xml content) out)))


;; Middleware ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 wrap-profiler)

(define/contract ((wrap-profiler handler) req)
  (-> (-> request? can-be-response?)
      (-> request? can-be-response?))
  (parameterize ([current-profile (make-profile)])
    (with-timing 'http (format "~a ~a"
                               (request-method req)
                               (url->string (request-uri req)))
      (handler req))))
