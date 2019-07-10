#lang racket/base

(require db
         net/url
         racket/contract
         racket/function
         racket/match
         racket/promise
         racket/string
         web-server/http
         "database.rkt")

;; db ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 truncate-tables!)

(define/contract (truncate-tables! db . tables)
  (-> database? (or/c string? symbol?) ... void?)
  (with-database-connection [conn db]
    (for ([table tables])
      (query-exec conn (format "truncate table ~a cascade" table)))))


;; http ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-test-request)

(define stringy/c
  (or/c string? bytes?))

(define maybe-stringy/c
  (or/c false/c stringy/c))

(define (stringy->bytes s)
  (cond
    [(bytes?  s) s]
    [(string? s) (string->bytes/utf-8 s)]))

(define (maybe-stringy->bytes s)
  (and s (stringy->bytes s)))

(define/contract (make-test-request #:method [method "GET"]
                                    #:content [content #f]
                                    #:headers [headers null]
                                    #:bindings [bindings null]
                                    #:scheme [scheme "http"]
                                    #:host [host "127.0.0.1"]
                                    #:port [port 80]
                                    #:path [path "/"]
                                    #:query [query null]
                                    #:client-ip [client-ip "127.0.0.1"])
  (->* ()
       (#:method maybe-stringy/c
        #:content maybe-stringy/c
        #:headers (listof (or/c header? (cons/c stringy/c stringy/c)))
        #:bindings (listof binding?)
        #:scheme string?
        #:host string?
        #:port (integer-in 0 65535)
        #:path string?
        #:query (listof (cons/c symbol? (or/c false/c string?)))
        #:client-ip string?)
       request?)

  (let ([method (maybe-stringy->bytes method)]
        [url (url scheme #f host port #t (map (curryr path/param null) (string-split path "/")) query #f)]
        [headers (for/list ([header headers])
                   (match header
                     [(? header?) header]
                     [(cons name value) (make-header (stringy->bytes name)
                                                     (stringy->bytes value))]))]
        [bindings (delay (append bindings (for/list ([param query])
                                            (make-binding:form (string->bytes/utf-8 (symbol->string (car param)))
                                                               (string->bytes/utf-8 (cdr param))))))]
        [content (maybe-stringy->bytes content)])
    (request method url headers bindings content host port client-ip)))
