#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/contract/base
         racket/format
         racket/match
         racket/string
         web-server/servlet
         xml
         "contract.rkt"
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
 timing-db-statements

 (contract-out
  [current-profile (parameter/c profile?)]
  [make-profile (-> profile?)]
  [profile? (-> any/c boolean?)]
  [profile-timings (-> profile? (listof timing?))]
  [profile-xexpr (-> profile? xexpr?)]
  [profile-write (->* [] [profile? output-port?] void?)]))

(define profiler-enabled?
  (make-parameter #f))

(struct timing (id parent label description duration db-statements))
(struct pdata (seq timings))
(struct profile (data))

(define (make-profile)
  (profile (box (pdata 0 null))))

(define profile-timings
  (compose1 pdata-timings unbox profile-data))

(define (profile-generate-id! p)
  (let ([next-id #f])
    (box-swap!
     (profile-data p)
     (lambda (data)
       (set! next-id (add1 (pdata-seq data)))
       (struct-copy pdata data [seq next-id])))
    next-id))

(define (profile-add-timing! p t)
  (box-swap!
   (profile-data p)
   (lambda (data)
     (define timings (cons t (pdata-timings data)))
     (struct-copy pdata data [timings timings]))))

(define (profile-find-roots p)
  (reverse
   (filter
    (lambda (t)
      (= (timing-parent t) 0))
    (profile-timings p))))

(define (profile-find-children p t)
  (reverse
   (filter
    (lambda (child-t)
      (= (timing-id t)
         (timing-parent child-t)))
    (profile-timings p))))

(define current-profile
  (make-parameter (make-profile)))

(define current-profile-label
  (make-parameter 'root))

(define current-timing-id
  (make-parameter 0))

(define (make-db-statement-collector predicate)
  (define collector
    (thread
     (lambda ()
       (define receiver
         (make-log-receiver
          (current-logger)
          'debug 'koyo:db-statements))
       (let loop ([statements null])
         (sync
          (handle-evt
           (thread-receive-evt)
           (lambda (_)
             (match-define `(stop ,ch)
               (thread-receive))
             (channel-put ch statements)))
          (handle-evt
           receiver
           (match-lambda
             [(vector _level _message `(,source-thread ,fsym ,stmt) _topic)
              (if (predicate source-thread fsym stmt)
                  (loop (cons stmt statements))
                  (loop statements))])))))))
  (lambda ()
    (define statements-ch (make-channel))
    (thread-send collector `(stop ,statements-ch))
    (channel-get statements-ch)))

(define (record-timing #:label [label (current-profile-label)]
                       #:description description
                       #:profile [the-profile (current-profile)]
                       proc)
  (define parent-id #f)
  (define current-id #f)
  (define start-time #f)
  (define get-statements #f)
  (dynamic-wind
    (lambda ()
      (set! parent-id (current-timing-id))
      (set! current-id (profile-generate-id! the-profile))
      (set! start-time (current-inexact-monotonic-milliseconds))
      (set! get-statements
            (make-db-statement-collector
             (let ([timing-thread (current-thread)])
               (lambda (query-thread _fsym _stmt)
                 (eq? query-thread timing-thread))))))
    (lambda ()
      (parameterize ([current-profile-label label]
                     [current-timing-id current-id])
        (proc)))
    (lambda ()
      (define duration (- (current-inexact-monotonic-milliseconds) start-time))
      (define statements (get-statements))
      (define current-timing (timing current-id parent-id label description duration statements))
      (profile-add-timing! the-profile current-timing))))

(define-syntax (with-timing stx)
  (syntax-parse stx
    [(_ (~optional label:expr #:defaults ([label #'(current-profile-label)]))
        description
        e ...+)
     #'(let ([proc (lambda () e ...)])
         (if (profiler-enabled?)
             (record-timing
              #:label label
              #:description description
              proc)
             (proc)))]))

(define (profile-xexpr p)
  (define db-statements-seq 0)
  (define db-statements-index ;; string? -> seq
    (make-hash))
  (define (get-db-statement-indexes stmts)
    (for/list ([stmt (in-list stmts)])
      (hash-ref!
       db-statements-index stmt
       (lambda ()
         (begin0 db-statements-seq
           (set! db-statements-seq (add1 db-statements-seq)))))))

  (define (format-duration duration)
    (~a (~r duration #:precision '(= 2)) "ms"))

  (define (render-timing t)
    (define toggle-id
      (format "uprofiler-timing-toggle-~a" (timing-id t)))
    (haml
     (.uprofiler-timing
      (:input.uprofiler-timing-toggle
       ([:id toggle-id]
        [:type "checkbox"]))
      (:label
       ([:for toggle-id])
       (:span.uprofiler-timing-label
        (symbol->string (timing-label t)))
       (:span.uprofiler-timing-description
        (:code (timing-description t)))
       (:span.uprofiler-timing-duration
        (format-duration (timing-duration t))))
      (unless (null? (timing-db-statements t))
        (haml
         (:a.uprofiler-timing-db-statements
          ([:href
            (format "javascript:$uprofiler.statements(~a)"
                    (string-join
                     (map number->string
                          (get-db-statement-indexes
                           (timing-db-statements t)))
                     ","))])
          (let ([n (length (timing-db-statements t))])
            (if (= n 1)
                "1 statement"
                (format "~a statements" n))))))
      ,@(map render-timing (profile-find-children p t)))))

  ;; Render the roots before the final xexpr to populate the db
  ;; statement index.
  (define roots
    (profile-find-roots p))
  (define rendered-roots
    (map render-timing roots))

  (haml
   (.uprofiler-content
    (:script
     #<<SCRIPT
window.$uprofiler = {
  statements: (...ids) => {
    ids.forEach(id => {
      const el = document.querySelector(`[data-uprofiler-db-statement-id='${id}']`);
      console.log(el.innerText);
    });
  }
};
SCRIPT
     )
    ,@(for/list ([(stmt idx) (in-hash db-statements-index)])
        (haml
         (:output
          ([:data-uprofiler-db-statement-id (~a idx)]
           [:hidden "hidden"])
          stmt)))
    (:input.uprofiler-toggle#uprofiler-toggle
     ([:type "checkbox"]))
    (:label.uprofiler-label
     ([:for "uprofiler-toggle"])
     (format-duration (apply + (map timing-duration roots))))
    (.uprofiler-timings
     (.uprofiler-timings-inner ,@rendered-roots)
     (:label.uprofiler-close
      ([:for "uprofiler-toggle"])
      "Close")))))

(define (profile-write [p (current-profile)]
                       [out (current-output-port)])
  (define content
    (profile-xexpr p))
  (when content
    (parameterize ([current-unescaped-tags html-unescaped-tags])
      (write-xexpr content out))))


;; Middleware ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [wrap-profiler middleware/c]))

(define ((wrap-profiler handler) req . args)
  (parameterize ([current-profile (make-profile)])
    (with-timing 'http (format "~a ~a"
                               (request-method req)
                               (url->string (request-uri req)))
      (apply handler req args))))
