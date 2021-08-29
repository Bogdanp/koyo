#lang racket/base

(require racket/format)

(provide
 (struct-out exn:fail:hasher)
 oops)

(struct exn:fail:hasher exn:fail ())

(define (oops who fmt . args)
  (raise
   (exn:fail:hasher
    (~a who ": " (apply format fmt args))
    (current-continuation-marks))))
