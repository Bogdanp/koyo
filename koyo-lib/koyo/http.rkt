#lang racket/base

(require net/url
         racket/contract
         racket/format
         racket/string
         web-server/http)

;; URL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 url-scrub)

(define/contract (url-scrub u)
  (-> url? url?)
  (struct-copy url u [path (for/list ([pp (in-list (url-path u))])
                             (path/param (path/param-path pp) null))]))


;; Requests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 request-path

 bindings-ref
 bindings-ref-bytes
 bindings-ref-number
 bindings-ref-symbol)

(define/contract (request-path req)
  (-> request? string?)
  (~a "/" (string-join (map path/param-path (url-path (request-uri req))) "/")))

(define/contract (bindings-ref bindings name [default #f])
  (->* ((listof binding?) symbol?) ((or/c false/c string?)) (or/c false/c string?))
  (cond
    [(bindings-assq (string->bytes/utf-8 (symbol->string name)) bindings)
     => (compose1 bytes->string/utf-8 binding:form-value)]

    [else default]))

(define ((make-bindings-reffer p) bindings name [default #f])
  (cond
    [(bindings-ref bindings name) => p]
    [else default]))

(define bindings-ref-bytes  (make-bindings-reffer string->bytes/utf-8))
(define bindings-ref-number (make-bindings-reffer string->number))
(define bindings-ref-symbol (make-bindings-reffer string->symbol))
