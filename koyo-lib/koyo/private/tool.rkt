#lang racket/base

(require racket/format
         racket/future
         racket/system
         version/utils)

(provide
 racket-exe
 raco-exe
 raco
 make!)

;; Before racket/racket@1bfb999, `make -j n --disable-constant`
;; silently discarded the `--disable-constant` flag, so we need this
;; check to avoid parallel compilation on those versions of Racket.
(define make-in-parallel?
  (not (version<? (version) "8.0.2.4")))

(define racket-exe (find-executable-path "racket"))
(define raco-exe (find-executable-path "raco"))
(define (raco . args)
  (begin0 (apply system*/exit-code raco-exe args)
    (flush-output (current-output-port))
    (flush-output (current-error-port))))

(define make!-sema (make-semaphore 1))
(define (make! path #:parallel? [parallel? make-in-parallel?])
  (call-with-semaphore make!-sema
    (lambda ()
      (zero?
       (if parallel?
           (raco "make" "--disable-constant" "-j" (~a (processor-count)) "-v" path)
           (raco "make" "--disable-constant" "-v" path))))))
