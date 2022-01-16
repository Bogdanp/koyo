#lang racket/base

(require racket/contract
         racket/match
         racket/path
         racket/port
         web-server/servlet
         "contract.rkt"
         "profiler.rkt")

(provide
 current-preload-dependencies
 track-preload-dependency!
 make-preload-headers
 (contract-out
  [wrap-preload middleware/c]))

(define/contract current-preload-dependencies
  (parameter/c (listof string?))
  (make-parameter null))

(define/contract (track-preload-dependency! dependency)
  (-> string? void?)
  (current-preload-dependencies (cons dependency (current-preload-dependencies))))

(define query-re
  #rx"\\?.*")

(define (path-remove-query p)
  (regexp-replace query-re p ""))

(define (path->preload-as p)
  (match (path-get-extension (path-remove-query p))
    [#".css" "style"]
    [#".js"  "script"]
    [#".mjs" "script"]
    [_       #f]))

(define/contract (make-preload-headers)
  (-> (listof header?))
  (with-timing 'preload "make-preload-headers"
    (for*/list ([path (in-list (current-preload-dependencies))]
                [preload-as (in-value (path->preload-as path))]
                #:when preload-as)
      (header #"Link" (with-output-to-bytes
                        (lambda ()
                          (display "<")
                          (display path)
                          (display ">; rel=preload; as=")
                          (display preload-as)))))))

(define ((wrap-preload handler) req . args)
  (with-timing 'preload "wrap-preload"
    (parameterize ([current-preload-dependencies null])
      (apply handler req args))))
