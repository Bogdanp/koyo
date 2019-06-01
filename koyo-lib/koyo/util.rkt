#lang racket/base

(require racket/contract)

(provide
 box-swap!)

(define/contract (box-swap! b f)
  (-> box? (-> any/c any/c) void?)
  (let loop ([v (unbox b)])
    (unless (box-cas! b v (f v))
      (loop (unbox b)))))
