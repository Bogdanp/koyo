#lang racket/base

(require net/url
         racket/contract
         racket/function
         racket/match
         racket/string)

;; External URLs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-application-url-scheme
 current-application-url-host
 current-application-url-port
 make-application-url)

(define/contract current-application-url-scheme
  (parameter/c (or/c "http" "https"))
  (make-parameter "http"))

(define/contract current-application-url-host
  (parameter/c non-empty-string?)
  (make-parameter "127.0.0.1"))

(define/contract current-application-url-port
  (parameter/c (integer-in 0 65535))
  (make-parameter 8000))

(define/contract (make-application-url #:query [query null]
                                       #:fragment [fragment #f]
                                       . path-elements)
  (->* ()
       (#:query (listof (cons/c symbol? string?))
        #:fragment (or/c false/c string?))
       #:rest (listof string?)
       string?)

  (define path
    (map (curryr path/param null) path-elements))

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
 current-reverse-uri-fn
 reverse-uri)

(define/contract current-reverse-uri-fn
  (parameter/c (-> symbol? any/c ... string?))
  (make-parameter (lambda (name . args)
                    (error "current-reverse-uri-fn not installed"))))

(define (reverse-uri . args)
  (apply (current-reverse-uri-fn) args))
