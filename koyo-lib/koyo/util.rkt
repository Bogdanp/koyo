#lang racket/base

(require racket/contract)

;; Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 box-swap!)

(define/contract (box-swap! b f)
  (-> box? (-> any/c any/c) void?)
  (let loop ([v (unbox b)])
    (unless (box-cas! b v (f v))
      (loop (unbox b)))))


;; Threads ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 call-with-timeout)

(define/contract (call-with-timeout f #:timeout [timeout 5000])
  (->* ((-> any/c))
       (#:timeout exact-nonnegative-integer?)
       (or/c 'timeout any/c))

  (define deadline (+ (current-inexact-milliseconds) timeout))
  (define chan (make-channel))
  (define worker
    (thread
     (lambda ()
       (channel-put chan (f)))))

  (sync/enable-break
   (choice-evt chan
               (wrap-evt
                (alarm-evt deadline)
                (lambda _
                  (begin0 'timeout
                    (kill-thread worker)))))))
