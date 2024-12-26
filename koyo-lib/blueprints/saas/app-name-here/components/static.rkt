#lang racket/base

(require koyo/preload
         koyo/url
         (prefix-in config: "../config.rkt"))

(provide
 static-uri)

(define (static-uri path)
  (define path/full (format "/static/~a?rev=~a" path config:version))
  (begin0 path/full
    (track-preload-dependency! path/full)))
