#lang racket/base

(require net/url
         racket/contract/base
         racket/format
         racket/match
         racket/string
         web-server/http)

;; URL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [url-scrub (-> url? url?)]
  [url-path* (-> url? string?)]
  [url-has-path? (-> url? string? boolean?)]))

(define (url-scrub u)
  (struct-copy url u [path (for/list ([pp (in-list (url-path u))])
                             (path/param (path/param-path pp) null))]))

(define (url-path* u)
  (string-join
   #:before-first "/"
   (map path/param-path (url-path u))
   "/"))

(define (url-has-path? u p)
  (define segments
    (string-split p "/"))
  (define path-params
    (match (url-path u)
      [(list (path/param "" _)) null]
      [params params]))
  (and (= (length path-params)
          (length segments))
       (for/and ([param (in-list path-params)]
                 [segment (in-list segments)])
         (string=? (path/param-path param) segment))))


;; Requests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [request-path (-> request? string?)]
  [bindings-ref (bindings-ref/c string?)]
  [bindings-ref-bytes (bindings-ref/c bytes?)]
  [bindings-ref-number (bindings-ref/c number?)]
  [bindings-ref-symbol (bindings-ref/c symbol?)]))

(define (request-path req)
  (~a "/" (string-join (map path/param-path (url-path (request-uri req))) "/")))

(define (bindings-ref/c res/c)
  (->* [(listof binding?) symbol?]
       [(or/c #f res/c)]
       (or/c #f res/c)))

(define (bindings-ref bindings name [default #f])
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
