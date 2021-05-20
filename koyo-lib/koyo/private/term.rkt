#lang racket/base

(require racket/match)

(provide
 make-color
 make-gray
 colorize)

(define number->bytes
  (compose1 string->bytes/utf-8 number->string))

(define (make-color r g b)
  (number->bytes (+ 16 (* 36 r) (* 6 g) b)))

(define (make-gray pct)
  (number->bytes (inexact->exact (+ 232 (* pct 24)))))

(define (bg color out)
  (write-bytes #"\e[48;5;" out)
  (write-bytes color out)
  (write-bytes #"m" out))

(define (fg color out)
  (write-bytes #"\e[38;5;" out)
  (write-bytes color out)
  (write-bytes #"m" out))

(define (color spec [out (current-output-port)])
  (for ([pair (in-list spec)])
    (match pair
      [(list 'fg color) (fg color out)]
      [(list 'bg color) (bg color out)])))

(define (reset [out (current-output-port)])
  (write-bytes #"\e[0m" out))

(define (call-with-colorized-output spec proc)
  (dynamic-wind
    (lambda () (color spec))
    (lambda () (proc))
    (lambda () (reset))))

(define-syntax-rule (colorize spec body0 body ...)
  (call-with-colorized-output spec (lambda () body0 body ...)))
