#lang racket/base

(require racket/match)

(provide
 (all-defined-out))

(define-logger worker)

(define (log-status j message . args)
  (match-define (vector id queue name _ attempts) j)
  (log-worker-debug
   "~a~n  id: ~s~n  queue: ~s~n  name: ~s~n  attempts: ~s"
   (apply format message args)
   id queue name attempts))

(define (log-update-error j e)
  (match-define (vector id queue name _ attempts) j)
  (log-worker-error
   "failed to update job~n  id: ~s~n  queue: ~s~n  name: ~s~n  attempts: ~s"
   id queue name attempts))
