#lang racket/base

(require racket/string)

(provide
 version-number)

(define (version-number [v (version)])
  (define parts
    (map string->number (string-split v ".")))
  (for/sum ([e (in-list '(10 7 4 0))]
            [p (in-sequences parts '(0 0 0 0))])
    (* p (expt 10 e))))
