#lang racket/base

(require racket/contract
         racket/match)

(provide
 make-color
 make-gray
 colorize)

(define color-axis/c
  (integer-in 0 6))

(define/contract (make-color r g b)
  (-> color-axis/c color-axis/c color-axis/c (integer-in 16 232))
  (+ 16 (* 36 r) (* 6 g) b))

(define/contract (make-gray pct)
  (-> (real-in 0 1) (integer-in 232 256))
  (inexact->exact (+ 232 (* pct 24))))

(define (bg color)
  (display "\e[48;5;")
  (display color)
  (display "m"))

(define (fg color)
  (display "\e[38;5;")
  (display color)
  (display "m"))

(define (color spec [out (current-output-port)])
  (parameterize ([current-output-port out])
    (for ([pair (in-list spec)])
      (match pair
        [(list 'fg color) (fg color)]
        [(list 'bg color) (bg color)]))))

(define (reset [out (current-output-port)])
  (display #"\e[0m" out))

(define (call-with-colorized-output spec proc)
  (dynamic-wind
    (lambda _ (color spec))
    (lambda _ (proc))
    (lambda _ (reset))))

(define-syntax-rule (colorize spec body0 body ...)
  (call-with-colorized-output spec (lambda _ body0 body ...)))

(module+ test
  (require racket/port
           rackunit)

  (check-equal?
   (with-output-to-string
     (lambda _
       (colorize
        `((fg ,(make-color 5 0 0)))
        (display "hello!"))))
   "\e[38;5;196mhello!\e[0m")

  (check-equal?
   (with-output-to-string
     (lambda _
       (colorize
        `((fg ,(make-color 5 0 0))
          (bg ,(make-color 5 5 5)))
        (display "hello!"))))
   "\e[38;5;196m\e[48;5;231mhello!\e[0m"))
