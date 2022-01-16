#lang racket/base

(require racket/contract
         racket/function
         racket/string
         web-server/http
         "contract.rkt"
         "profiler.rkt"
         "url.rkt")

(provide
 current-cors-origin
 current-cors-methods
 current-cors-headers
 current-cors-max-age
 current-cors-credentials-allowed?
 (contract-out
  [wrap-cors middleware/c]))

(define/contract current-cors-origin
  (parameter/c (or/c #f non-empty-string?))
  (make-parameter #f))

(define/contract current-cors-methods
  (parameter/c (listof non-empty-string?))
  (make-parameter '("HEAD" "DELETE" "GET" "PATCH" "POST" "PUT" "OPTIONS")))

(define/contract current-cors-headers
  (parameter/c (listof non-empty-string?))
  (make-parameter (list "*")))

(define/contract current-cors-max-age
  (parameter/c exact-nonnegative-integer?)
  (make-parameter 86400))

(define/contract current-cors-credentials-allowed?
  (parameter/c boolean?)
  (make-parameter #t))

(define (make-allow-origin-header)
  (define origin
    (string->bytes/utf-8
     (cond
       [(current-cors-origin)
        => identity]

       [else
        (format "~a://~a"
                (current-application-url-scheme)
                (current-application-url-host))])))

  (make-header #"Access-Control-Allow-Origin" origin))

(define (make-options-headers)
  (define headers
    (list (make-allow-origin-header)
          (make-header #"Access-Control-Allow-Methods"
                       (string->bytes/utf-8
                        (string-join (current-cors-methods) ",")))
          (make-header #"Access-Control-Allow-Headers"
                       (string->bytes/utf-8
                        (string-join (current-cors-headers) ",")))
          (make-header #"Access-Control-Max-Age"
                       (string->bytes/utf-8
                        (number->string (current-cors-max-age))))))

  (if (current-cors-credentials-allowed?)
      (cons (make-header #"Access-Control-Allow-Credentials" #"true") headers)
      headers))

(define ((wrap-cors handler) req . args)
  (with-timing 'cors "wrap-cors"
    (cond
      [(bytes=? (request-method req) #"OPTIONS")
       (response/full 200 #"OK" (current-seconds) #f (make-options-headers) null)]

      [else
       (define resp (apply handler req args))
       (define headers (cons (make-allow-origin-header)
                             (response-headers resp)))

       (struct-copy response resp [headers headers])])))
