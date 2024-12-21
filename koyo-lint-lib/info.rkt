#lang info

(define license 'BSD-3-Clause)
(define collection "koyo")
(define deps
  '("base"
    "review"))
(define review-exts
  '((koyo/review should-review-syntax? review-syntax)))
