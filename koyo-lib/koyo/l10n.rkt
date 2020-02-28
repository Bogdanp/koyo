#lang racket/base

(require gregor
         racket/contract
         racket/match
         racket/string
         srfi/29
         web-server/http
         "profiler.rkt"
         "session.rkt")

;; Translate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-locale-specifier
 current-locales
 load-locales!

 translate
 localize-date)

(define/contract current-locale-specifier
  (parameter/c symbol?)
  (make-parameter 'koyo))

(define/contract current-locales
  (parameter/c (listof (cons/c symbol? symbol?)))
  (make-parameter null))

(define/contract (load-locales! locales-path)
  (-> path-string? void?)
  (current-locales
   (for*/list ([language-dir (directory-list locales-path)]
               [country-file (directory-list (build-path locales-path language-dir))])
     (define language (string->symbol (path->string language-dir)))
     (define country (string->symbol (path->string (path-replace-extension country-file ""))))
     (define specifier (list (current-locale-specifier) language country))
     (declare-bundle! specifier (call-with-input-file (build-path locales-path language-dir country-file) read))
     (cons language country))))

(define/contract (translate message-name . args)
  (-> symbol? any/c ... string?)
  (cond
    [(localized-template (current-locale-specifier) message-name)
     => (lambda (message)
          (apply format message args))]

    [else (symbol->string message-name)]))

(define/contract (localize-date d)
  (-> date-provider? string?)
  (match (current-language)
    ['ro (~t d "dd MMMM, yyyy")]
    ['de (~t d "dd.MM.yyyy")]
    [_   (~t d "MMMM dd, yyyy")]))


;; Middleware ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 language-header->locale
 wrap-browser-locale)

(define accept-language-header
  #"accept-language")

(define language-spec-re
  #px"\\s*([^-]+)(?:-([^\\s;]+))?(?:;q=([\\d]+))?\\s*")

(define/contract (((wrap-browser-locale sessions) handler) req)
  (-> session-manager? (-> (-> request? response?)
                           (-> request? response?)))

  (with-timing 'http "wrap-browser-locale"
    (define accept-language
      (bytes->string/utf-8
       (cond
         [(headers-assq* accept-language-header (request-headers/raw req)) => header-value]
         [else #"en-US"])))

    (define user-language
      (session-manager-ref sessions 'l10n.lang accept-language))

    (match-define (cons language country)
      (or (language-header->locale user-language) '(en . us)))

    (parameterize ([current-language language]
                   [current-country country]
                   [current-locale (format "~a_~a.UTF-8" language country)])
      (handler req))))

(define (make-locale-pair language country)
  (cons (string->symbol (string-downcase language))
        (string->symbol (string-downcase (or country language)))))

(define/contract (language-header->locale header)
  (-> string? (or/c false/c string?))
  (define specs
    (for/list ([spec (string-split header ",")])
      (match-define (list _ language country weight)
        (regexp-match language-spec-re spec))

      (cons (make-locale-pair language country)
            (string->number (or weight "1")))))

  (for/first ([spec (sort specs > #:key cdr)]
              #:when (member (car spec) (current-locales)))
    (car spec)))
