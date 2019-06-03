#lang racket/base

(require gregor
         mzlib/os
         racket/contract
         racket/format
         racket/list
         racket/logging
         racket/match)

(provide
 start-logger)

(define/contract (start-logger #:levels levels
                               #:parent [parent (current-logger)]
                               #:output-port [out (current-error-port)])
  (->* (#:levels (listof (cons/c symbol? log-level/c)))
       (#:parent logger?
        #:output-port port?)
       (-> void?))

  (define stopped (make-semaphore))
  (define receiver
    (apply make-log-receiver parent
           (flatten
            (for/list ([level (in-list levels)])
              (list (cdr level) (car level))))))

  (define (receive-logs)
    (sync
     (choice-evt
      (handle-evt receiver
                  (match-lambda
                    [(vector level message _ _)
                     (fprintf out
                              "[~a] [~a] [~a] ~a\n"
                              (~t (now) "yyyy-MM-dd HH:mm:ss")
                              (~a (getpid) #:align 'right #:width 8)
                              (~a level #:align 'right #:width 7)
                              message)
                     (receive-logs)]))
      stopped)))

  (define thd
    (thread receive-logs))

  (lambda ()
    (sync (system-idle-evt))
    (semaphore-post stopped)
    (void (sync thd))))
