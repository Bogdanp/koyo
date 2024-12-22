#lang racket/base

(require json
         net/url
         racket/contract/base
         racket/format
         racket/match
         racket/string
         threading
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
  [request-headers-ref (-> request? bytes? (or/c #f bytes?))]
  [request-headers-ref* (-> request? bytes? (or/c #f string?))]
  [request-ip-address (-> request? string?)]
  [request-json (-> request? jsexpr?)]
  [request-path (-> request? string?)]
  [request-reroot (-> request? url? (or/c #f request?))]
  [bindings-ref (bindings-ref/c string?)]
  [bindings-ref-bytes (bindings-ref/c bytes?)]
  [bindings-ref-number (bindings-ref/c number?)]
  [bindings-ref-symbol (bindings-ref/c symbol?)]))

(define (request-headers-ref req name)
  (and~>
   (request-headers/raw req)
   (headers-assq* name _)
   (header-value)))

(define (request-headers-ref* req name)
  (and~>
   (request-headers-ref req name)
   (bytes->string/utf-8)))

(define (request-ip-address req)
  (or
   (and~>
    (request-headers-ref req #"x-forwarded-for")
    (regexp-split #rx"," _)
    (car))
   (request-headers-ref req #"x-real-ip")
   (request-client-ip req)))

(define (request-json req)
  (define data (request-post-data/raw req))
  (and data (bytes->jsexpr data)))

(define (request-path req)
  (~a "/" (string-join (map path/param-path (url-path (request-uri req))) "/")))

(define (request-reroot req root)
  (let loop ([root (url-path root)]
             [path (url-path (request-uri req))])
    (match* (root path)
      [((list) path)
       (let* ([path (if (null? path) (list (path/param "" null)) path)]
              [url (struct-copy url (request-uri req) [path path])])
         (struct-copy request req [uri url]))]
      [(_ (list)) #f]
      [((list path-segment root ...)
        (list path-segment path ...))
       (loop root path)]
      [(_ _) #f])))

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
