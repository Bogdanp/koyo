#lang racket/base

(require net/uri-codec
         net/url
         racket/contract/base
         racket/match
         racket/string)

;; External URLs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [current-application-url-scheme (parameter/c (or/c "http" "https"))]
  [current-application-url-host (parameter/c non-empty-string?)]
  [current-application-url-port (parameter/c (integer-in 0 65535))]
  [make-application-url
   (->* []
        [#:query (listof (cons/c symbol? string?))
         #:fragment (or/c #f string?)]
        #:rest (listof string?)
        string?)]))

(define current-application-url-scheme
  (make-parameter "http"))

(define current-application-url-host
  (make-parameter "127.0.0.1"))

(define current-application-url-port
  (make-parameter 8000))

(define (make-application-url #:query [query null]
                              #:fragment [fragment #f]
                              . path-elements)
  (define path
    (for/list ([elt (in-list path-elements)])
      (path/param elt null)))
  (define port
    (match (current-application-url-port)
      [80   #f]
      [443  #f]
      [port port]))

  (url->string
   (url (current-application-url-scheme)
        #f
        (current-application-url-host)
        port
        #t
        path
        query
        fragment)))


;; Internal URLs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [current-reverse-uri-fn
   (parameter/c (-> symbol? any/c ... string?))]
  [reverse-uri
   (->* [symbol?]
        [#:query (listof (cons/c symbol? (or/c #f string?)))]
        #:rest any/c
        string?)]))

(define current-reverse-uri-fn
  (make-parameter (lambda (_name . _args)
                    (error "current-reverse-uri-fn not installed"))))

(define (reverse-uri where #:query [query null] . args)
  (define uri (apply (current-reverse-uri-fn) where args))
  (cond
    [(null? query) uri]
    [else (format "~a?~a" uri (alist->form-urlencoded query))]))
