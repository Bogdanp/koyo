#lang racket/base

(require racket/format
         racket/future
         racket/system)

(provide
 racket-exe
 raco-exe
 raco
 make!)

(define racket-exe (find-executable-path "racket"))
(define raco-exe (find-executable-path "raco"))
(define (raco . args)
  (apply system*/exit-code raco-exe args))

(define make!-sema (make-semaphore 1))
(define (make! path #:parallel? [parallel? #t])
  (call-with-semaphore make!-sema
    (lambda ()
      (zero?
       (if parallel?
           (raco "make" "--disable-constant" "-j" (~a (processor-count)) path)
           (raco "make" "--disable-constant" "-v" path))))))
