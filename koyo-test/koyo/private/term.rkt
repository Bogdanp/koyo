#lang racket/base

(require koyo/private/term
         racket/port
         rackunit)

(provide
 term-tests)

(define term-tests
  (test-suite
   "term"

   (test-suite
    "colorize"

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
     "\e[38;5;196m\e[48;5;231mhello!\e[0m"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests term-tests))
