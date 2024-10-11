#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         (prefix-in h: html)
         json
         racket/contract/base
         racket/match
         racket/port
         racket/promise
         racket/runtime-path
         racket/set
         racket/string
         xml)

(provide
 (contract-out
  [html->xexpr
   (->* [string?]
        [(-> string? string?)]
        (listof xexpr?))]
  [html->xexpr/first
   (-> string? xexpr?)]
  [xexpr->text
   (->* [xexpr?]
        [string?]
        string?)])
 xexpr-attr-ref
 xexpr-attr-ref*
 xexpr-children
 xexpr-select
 xexpr-select-first
 xexpr-select-first*
 xexpr-select-text
 xexpr-unless
 xexpr-when)

;; https://html.spec.whatwg.org/multipage/named-characters.html#named-character-references
(define-runtime-path html-entities.json
  "xexpr/html-entities.json")

(define html-entities
  (delay/sync
   (call-with-input-file html-entities.json
     (lambda (in)
       (for/hasheq ([(entity data) (in-hash (read-json in))])
         (define str (symbol->string entity))
         (define sym (string->symbol (substring str 1 (sub1 (string-length str)))))
         (values sym (hash-ref data 'characters)))))))

(define (default-surround s)
  (string-append "<div>" s "</div>"))

(define (html->xexpr s [surround default-surround])
  (call-with-input-string (surround s)
    (lambda (in)
      (map xml->xexpr (h:read-html-as-xml in)))))

(define html->xexpr/first
  (compose1 car html->xexpr))

(define-syntax-rule (define-xexpr-attr-ref id for/form not-found-expr)
  (define (id xexpr attr-id)
    (match xexpr
      [(list _ (? xexpr-attr-list? attrs) _ (... ...)) ;; noqa
       (for/form ([pair (in-list attrs)]
                  [attr (in-value (car pair))]
                  #:when (eq? attr attr-id))
         (cadr pair))]
      [_ not-found-expr])))

(define-xexpr-attr-ref xexpr-attr-ref for*/first #f)
(define-xexpr-attr-ref xexpr-attr-ref* for*/list null)

(define (xexpr-children e)
  (match e
    [(or (list _ (? xexpr-attr-list?) es ...)
         (list _ es ...)) es] ;; noqa
    [_ null]))

(define (xexpr-attr-list? lst)
  (or (null? lst)
      (and (pair? lst)
           (for/and ([p (in-list lst)])
             (and (pair? p)
                  ((length p) . = . 2)
                  (symbol? (car p))
                  (string? (cadr p)))))))

(define (xexpr->text e [sep " "])
  (match e
    [(? string?) e]

    [(? symbol?)
     (hash-ref
      #;ht (force html-entities)
      #;key e
      #;fail (λ () (format "&~a;" e)))]

    [(? number?)
     (if (or (and (>= e 0)
                  (<= e #xD7FF))
             (and (>= e #xE000)
                  (<= e #x10FFFF)))
         (string (integer->char e))
         (string #\uFFFD))]

    [(list _ (? xexpr-attr-list?) e ...) ;; noqa
     (string-join (map xexpr->text e) sep)]

    [(list _ e ...) ;; noqa
     (string-join (map xexpr->text e) sep)]

    [(cdata _ _ s)
     ;; <![CDATA[...]]>
     (substring s 9 ((string-length s) . - . 3))]))

(define (attribute-pairs->hash xs)
  (for/hash ([pair (in-list xs)])
    (match-define (list attribute value) pair)
    (values attribute (list->set (string-split value " ")))))

(define (attributes-subset? xs ys)
  (define xs:hash (attribute-pairs->hash xs))
  (define ys:hash (attribute-pairs->hash ys))
  (for/and ([(name value) (in-hash ys:hash)])
    (cond
      [(hash-ref xs:hash name #f)
       => (λ (attrs) (subset? value attrs))]
      [else #f])))

(define xexpr-select-matches?
  (match-lambda**
   [((list '* selector-attrs)
     (list _ element-attrs e ...))
    (attributes-subset? (if (xexpr-attr-list? element-attrs) element-attrs null) selector-attrs)]

   [((list tag selector-attrs)
     (list tag element-attrs e ...))
    (attributes-subset? (if (xexpr-attr-list? element-attrs) element-attrs null) selector-attrs)]

   [(_ _) #f]))

(define (xexpr-selector-select selector e)
  (let loop ([e e])
    (if (xexpr-select-matches? selector e)
        (list e)
        (apply append (map loop (xexpr-children e))))))

(define ((make-xexpr-selector selectors) xexpr) ;; noqa
  (let loop ([selectors selectors]
             [xexpr xexpr])
    (match selectors
      [(list) null]
      [(list selector)
       (xexpr-selector-select selector xexpr)]
      [(list selector selectors ...) ;; noqa
       (apply
        append
        (let ([matches (xexpr-selector-select selector xexpr)])
          (for*/list ([m (in-list matches)]
                      [e (in-list (xexpr-children m))])
            (loop selectors e))))])))

(begin-for-syntax
  (define-syntax-class selector
    (pattern tag:id
             #:with attrs #'())

    (pattern (tag:id [(attr:id value:str) ...+])
             #:with attrs #'((attr value) ...))))

(define-syntax (xexpr-select stx)
  (syntax-parse stx
    [(_ e:expr s:str)
     #'((make-xexpr-selector (parse-selectors s)) e)]

    [(_ e:expr s:selector ...+)
     #'((make-xexpr-selector (list (list 's.tag 's.attrs) ...)) e)]))

(define-syntax (xexpr-select-first stx)
  (syntax-parse stx
    [(_ e:expr s ...+)
     #'(car (xexpr-select e s ...))]))

(define-syntax (xexpr-select-first* stx)
  (syntax-parse stx
    [(_ e:expr s ...+)
     #'(let ([tmp (xexpr-select e s ...)])
         (if (null? tmp) #f (car tmp)))]))

(define-syntax (xexpr-select-text stx)
  (syntax-parse stx
    [(_ e:expr s ...+)
     #'(map (λ (e) (xexpr->text e ""))
            (xexpr-select e s ...))]))

(define-syntax (xexpr-unless stx)
  (syntax-parse stx
    [(_ condition:expr e:expr ...+)
     #'(if condition
           (list)
           (list e ...))]))

(define-syntax (xexpr-when stx)
  (syntax-parse stx
    [(_ condition:expr e:expr ...+)
     #'(if condition
           (list e ...)
           (list))]))


;; dynamic selectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (alphanumeric? c)
  (or (char=? c #\-)
      (char=? c #\_)
      (char-alphabetic? c)
      (char-numeric? c)))

(define (read-while p)
  (let loop ([next (peek-char)])
    (when (and (not (eof-object? next)) (p next))
      (display (read-char))
      (loop (peek-char)))))

(define (read-tag)
  (string->symbol
   (with-output-to-string
     (lambda ()
       (read-while alphanumeric?)))))

(define (read-class)
  (with-output-to-string
    (lambda ()
      (unless (char=? #\. (peek-char))
        (raise-user-error 'read-class "classes must start with a period"))

      (read-char)
      (read-while alphanumeric?))))

(define (read-id)
  (with-output-to-string
    (lambda ()
      (unless (char=? #\# (peek-char))
        (raise-user-error 'read-id "ids must start with a pound sign"))

      (read-char)
      (read-while alphanumeric?))))

(struct dyn:selector (tag attributes)
  #:transparent)

(define (set-tag s t)
  (struct-copy dyn:selector s [tag t]))

(define (append-attribute s attr)
  (struct-copy dyn:selector s [attributes (append (dyn:selector-attributes s) (list attr))]))

(define (parse-selectors s) ;; noqa
  (define selectors
    (with-input-from-string s
      (lambda ()
        (let loop ([selector (dyn:selector #f null)]
                   [selectors null])
          (match (peek-char)
            [(? eof-object?) (reverse (cons selector selectors))]
            [#\. (loop (append-attribute selector (cons 'class (read-class))) selectors)]
            [#\# (loop (append-attribute selector (cons 'id (read-id))) selectors)]

            [#\space
             (void (read-char))
             (loop (dyn:selector #f null)
                   (cons selector selectors))]

            [_
             (loop (set-tag selector (read-tag)) selectors)])))))

  (for/list ([selector (in-list selectors)])
    (define attributes
      (for/fold ([attributes (hash)])
                ([pair (in-list (dyn:selector-attributes selector))])
        (match-define (cons attribute value) pair)
        (hash-update attributes attribute (λ (vs) (cons value vs)) null)))

    `(,(or (dyn:selector-tag selector) '*)
      (,@(for/list ([(name value) (in-hash attributes)])
           `(,name ,(string-join (reverse value) " ")))))))
