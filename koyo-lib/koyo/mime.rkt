#lang racket/base

(require (for-syntax racket/base)
         racket/runtime-path
         web-server/private/mime-types)

(provide
 path->mime-type)

(define-runtime-path mime.types-path
  (build-path "mime.types"))

(define path->mime-type
  (make-path->mime-type mime.types-path))
