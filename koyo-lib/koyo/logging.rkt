#lang racket/base

(require mzlib/os
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

(define (current-formatted-date)
  (define ms (current-milliseconds))
  (define d (seconds->date (/ ms 1000)))
  (define (write-number n out)
    (write-string (number->string n) out))
  (define (write-padded-number n out [m 10])
    (let loop ([m m])
      (unless (zero? m)
        (when (< n m)
          (write-char #\0 out)
          (loop (quotient m 10)))))
    (write-number n out))
  (call-with-output-string
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
         (fprintf out
                  "[~a] [~a] [~a] ~a\n"
                  (current-formatted-date)
                  (~a pid #:align 'right #:width 8)
                  (with-output-to-string
                    (lambda _
                      (colorize
                       (case level
                         [(debug)   `((fg ,(make-color 0 0 4)))]
                         [(info)    `((fg ,(make-color 0 3 0)))]
                         [(warning) `((fg ,(make-color 3 1 0)))]
                         [(error)   `((fg ,(make-color 3 0 0)))]
                         [else      null])
                       (display (~a level #:align 'right #:width 7)))))
                  message)
         (receive-logs)]))))

  (define thd
    (thread receive-logs))

  (lambda ()
    (sync (system-idle-evt))
    (semaphore-post stopped)
    (void (sync thd))))
