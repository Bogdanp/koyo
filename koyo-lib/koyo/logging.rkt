#lang racket/base

(require gregor
         mzlib/os
         racket/contract
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
         (fprintf out
                  "[~a] [~a] [~a] ~a\n"
                  (~t (now) "yyyy-MM-dd HH:mm:ss")
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
