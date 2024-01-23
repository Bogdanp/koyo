#lang racket/base

(require racket/contract/base
         racket/generic)

(provide
 hasher?
 gen:hasher
 (contract-out
  [hasher-make-hash (-> hasher? string? string?)]
  [hasher-hash-matches? (-> hasher? string? string? boolean?)]))

(define-generics hasher
  (hasher-make-hash hasher pass)
  (hasher-hash-matches? hasher pass-hash pass))
