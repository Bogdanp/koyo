#lang racket/base

(require mzlib/os
         racket/contract/base
         racket/format
         racket/list
         racket/logging
         racket/match
         racket/port
         "private/term.rkt")

(provide
 (contract-out
  [start-logger
   (->* [#:levels (listof (cons/c symbol? log-level/c))]
        [#:color? boolean?
         #:parent logger?
         #:output-port port?]
        (-> void?))]))

(define (start-logger #:levels levels
                      #:color? [color? #t]
                      #:parent [parent (current-logger)]
                      #:output-port [out (current-error-port)])
  (define stopped (make-semaphore))
  (define receiver
    (apply make-log-receiver parent
           (flatten
            (for/list ([level (in-list levels)])
              (list (cdr level) (car level))))))

  (define preformatted-pid
    (string->bytes/utf-8
     (~a (getpid) #:align 'right #:width 8)))

  (define (format-level level)
    (with-output-to-bytes
      (lambda ()
        (colorize
         (case (and color? level)
           [(debug)   `((fg ,(make-color 0 0 4)))]
           [(info)    `((fg ,(make-color 0 3 0)))]
           [(warning) `((fg ,(make-color 3 1 0)))]
           [(error)   `((fg ,(make-color 3 0 0)))]
           [else      null])
         (write-string (~a level #:align 'right #:width 7))))))
  (define-syntax-rule (define-level-writer id [level ...])
    (begin
      (define level (format-level 'level)) ...
      (define (id sym [out (current-output-port)])
        (write-bytes
         (case sym
           [(level) level] ...
           [else (format-level sym)])
         out))))
  (define-level-writer write-level
    [debug info warning error])

  (define (receive-logs)
    (sync
     stopped
     (handle-evt
      receiver
      (match-lambda
        [(vector level message _ _)
         (write-bytes #"[" out)
         (write-timestamp out)
         (write-bytes #"] [" out)
         (write-bytes preformatted-pid out)
         (write-bytes #"] [" out)
         (write-level level out)
         (write-bytes #"] " out)
         (write-string message out)
         (write-char #\newline out)
         (receive-logs)]))))

  (define thd
    (thread receive-logs))

  (lambda ()
    (sync (system-idle-evt))
    (semaphore-post stopped)
    (void (sync thd))))

(define (write-timestamp out)
  (define ms (current-milliseconds))
  (define d (seconds->date (/ ms 1000)))
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
  (write-padded-number (modulo ms 1000) out 100))

(define (write-number n out)
  (write-string (number->string n) out))

(define (write-padded-number n out [m 10])
  (let loop ([m m])
    (unless (< m 10)
      (when (< n m)
        (write-char #\0 out)
        (loop (quotient m 10)))))
  (write-number n out))
