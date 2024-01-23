#lang racket/base

(require racket/contract/base)

;; Boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [box-swap! (-> box? (-> any/c any/c) void?)]))

(define (box-swap! b f)
  (let loop ([v (unbox b)])
    (unless (box-cas! b v (f v))
      (loop (unbox b)))))


;; Threads ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [call-with-timeout
   (->* [(-> any/c)]
        [#:timeout exact-nonnegative-integer?]
        (or/c 'timeout any/c))]))

(define (call-with-timeout f #:timeout [timeout 5000])
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
                (lambda (_)
                  (begin0 'timeout
                    (kill-thread worker)))))))
