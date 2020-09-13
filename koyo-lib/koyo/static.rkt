#lang racket/base

(require net/url
         racket/contract
         racket/list
         racket/string
         web-server/dispatchers/dispatch
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         web-server/dispatchers/filesystem-map
         "mime.rkt")

(provide
 make-static-dispatcher)

(define/contract (make-static-dispatcher root-path [root "/static/"])
  (->* (path-string?) (non-empty-string?) dispatcher/c)
  (define prefix-length (length (string-split root "/")))
  (define prefix-re (regexp (string-append "^" root ".+$")))
  (define url->path
    (make-url->path root-path))
  (define (static-url->path u)
    (url->path (struct-copy url u [path (drop (url-path u) prefix-length)])))
  (define static-dispatcher
    (files:make
     #:url->path static-url->path
     #:path->mime-type path->mime-type))
  (filter:make prefix-re static-dispatcher))
