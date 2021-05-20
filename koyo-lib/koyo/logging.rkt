#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         mzlib/os
         racket/contract
         racket/date
         racket/format
         racket/list
         racket/logging
         racket/match
         racket/port
         "private/term.rkt")

(provide
 start-logger)

(define/contract (start-logger #:levels levels
                               #:parent [parent (current-logger)]
                               #:output-port [out (current-error-port)])
  (->* (#:levels (listof (cons/c symbol? log-level/c)))
       (#:parent logger?
        #:output-port port?)
       (-> void?))

  (define pid (getpid))
  (define stopped (make-semaphore))
  (define receiver
    (apply make-log-receiver parent
           (flatten
            (for/list ([level (in-list levels)])
              (list (cdr level) (car level))))))

  (define (receive-logs)
    (sync
     stopped
     (handle-evt
      receiver
      (match-lambda
        [(vector level message _ _)
         (cprintf out
                  #"[~a] [~a] [~a] ~a\n"
                  (current-formatted-date)
                  (~a pid #:align 'right #:width 8)
                  (with-output-to-bytes
                    (lambda ()
                      (colorize
                       (case level
                         [(debug)   `((fg ,(make-color 0 0 4)))]
                         [(info)    `((fg ,(make-color 0 3 0)))]
                         [(warning) `((fg ,(make-color 3 1 0)))]
                         [(error)   `((fg ,(make-color 3 0 0)))]
                         [else      null])
                       (write-string (~a level #:align 'right #:width 7)))))
                  message)
         (receive-logs)]))))

  (define thd
    (thread receive-logs))

  (lambda ()
    (sync (system-idle-evt))
    (semaphore-post stopped)
    (void (sync thd))))

(define-syntax (cprintf stx)
  (define (parse-fmt bs)
    (filter-not
     (lambda (sub)
       (equal? sub #""))
     (add-between (regexp-split #rx#"~a" bs) 'arg)))

  (syntax-parse stx
    [(_ out-e:expr fmt:bytes arg-e:expr ...)
     #:with out-id (datum->syntax #'out-e 'out)
     #:with (write-e ...) (for/fold ([exprs null]
                                     [args (syntax-e #'(arg-e ...))]
                                     #:result (reverse exprs))
                                    ([chunk (in-list (parse-fmt (syntax->datum #'fmt)))])
                            (if (eq? chunk 'arg)
                                (values
                                 (cons #`(display #,(car args) out-id) exprs)
                                 (cdr args))
                                (values
                                 (cons #`(write-bytes #,(datum->syntax #'fmt chunk) out-id) exprs)
                                 args)))
     #'(let ([out-id out-e])
         write-e ...)]))

(define (current-formatted-date)
  (define ms (current-milliseconds))
  (define d (seconds->date (/ ms 1000)))
  (define (write-number n out)
    (write-string (number->string n) out))
  (define (write-padded-number n out [m 10])
    (let loop ([m m])
      (unless (< m 10)
        (when (< n m)
          (write-char #\0 out)
          (loop (quotient m 10)))))
    (write-number n out))
  (call-with-output-bytes
   (lambda (out)
     (write-number (date-year d) out)
     (write-char #\- out)
     (write-padded-number (date-month d) out)
     (write-char #\- out)
     (write-padded-number (date-day d) out)
     (write-char #\space out)
     (write-padded-number (date-hour d) out)
     (write-char #\: out)
     (write-padded-number (date-minute d) out)
     (write-char #\: out)
     (write-padded-number (date-second d) out)
     (write-char #\. out)
     (write-padded-number (modulo ms 1000) out 100))))
