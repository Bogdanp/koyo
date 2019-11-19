#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         db
         racket/file
         racket/runtime-path)

(define-runtime-path queries
  (build-path "queries"))

(define (make-virtual-stmt name)
  (define path (build-path queries (format "~a.sql" name)))
  (virtual-statement (file->string path)))

(define-syntax (define-stmt stx)
  (syntax-parse stx
    [(_ name:id)
     (with-syntax ([id (format-id #'name "~a-stmt" #'name)])
       #'(begin
           (define id (make-virtual-stmt 'name))
           (provide id)))]))

(define-stmt dequeue)
(define-stmt enqueue)
(define-stmt heartbeat)
(define-stmt mark-done)
(define-stmt mark-failed)
(define-stmt mark-for-retry)
(define-stmt register-worker)
(define-stmt unregister-stale-workers)
(define-stmt unregister-worker)
