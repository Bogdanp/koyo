#lang racket/base

(require (for-syntax racket/base
                     racket/function
                     syntax/parse/pre)
         (prefix-in h: html)
         racket/contract/base
         racket/function
         racket/match
         racket/port
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
   (-> string? xexpr?)])

 (contract-out
  [xexpr->text
   (->* [xexpr?]
        [string?]
        string?)])
 xexpr-select
 xexpr-select-first
 xexpr-select-text
 xexpr-unless
 xexpr-when)

(define (default-surround s)
  (string-append "<div>" s "</div>"))

(define (html->xexpr s [surround default-surround])
  (call-with-input-string (surround s)
    (lambda (in)
      (map xml->xexpr (h:read-html-as-xml in)))))

(define html->xexpr/first
  (compose1 car html->xexpr))

(define (xexpr->text e [sep " "])
  (match e
    [(? symbol?) (format "&~a;" e)]
    [(? number?) (format "&#~a;" e)]

    [(and s (? string?)) s]

    [(list _ (list (list (? symbol?) (? string?)) ...) e ...)
     (string-join (map xexpr->text e) sep)]

    [(list _ e ...)
     (string-join (map xexpr->text e) sep)]))

(define (attribute-pairs->hash xs)
  (for/hash ([pair (in-list xs)])
    (values (car pair) (list->set (string-split (cadr pair) " ")))))

(define (attributes-subset? xs ys)
  (define xs:hash (attribute-pairs->hash xs))
  (define ys:hash (attribute-pairs->hash ys))

  (for/fold ([res #t])
            ([(name value) (in-hash ys:hash)])
    (and res (cond
               [(hash-ref xs:hash name #f) => (curry subset? value)]
               [else #f]))))

(define attribute-list?
  (non-empty-listof (list/c any/c any/c)))

(define xexpr-select-matches?
  (match-lambda**
   [((list '* selector-attrs)
     (list _ element-attrs e ...))
    (attributes-subset? (if (attribute-list? element-attrs) element-attrs null) selector-attrs)]

   [((list tag selector-attrs)
     (list tag element-attrs e ...))
    (attributes-subset? (if (attribute-list? element-attrs) element-attrs null) selector-attrs)]

   [(_ _) #f]))

(define (xexpr-selector-select selector xexpr)
  (define matches?
    (curry xexpr-select-matches? selector))

  (let loop ([selected null]
             [remaining (list xexpr)])
    (cond
      [(null? remaining)
       (reverse selected)]

      [else
       (define-values (selected* remaining*)
         (for/fold ([selected selected]
                    [remaining null])
                   ([xexpr remaining])
           (match xexpr
             [(? matches?)
              (values (cons xexpr selected) remaining)]

             [(or (list _ (list (cons _ _) ...) es ...)
                  (list _ es ...))
              (values selected (append remaining es))]

             [_
              (values selected remaining)])))

       (loop selected* remaining*)])))

(define ((make-xexpr-selector selectors) xexpr)
  (let loop ([selectors selectors]
             [selected (list xexpr)])
    (match selectors
      [(list) selected]
      [(cons selector selectors)
       (loop selectors
             (apply append (for/list ([xexpr selected])
                             (xexpr-selector-select selector xexpr))))])))

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

(define-syntax (xexpr-select-text stx)
  (syntax-parse stx
    [(_ e:expr s ...+)
     #'(map (curryr xexpr->text "") (xexpr-select e s ...))]))

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

(define (parse-selectors s)
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
      (for*/fold ([attributes (hash)])
                 ([pair      (in-list (dyn:selector-attributes selector))]
                  [attribute (in-value (car pair))]
                  [value     (in-value (cdr pair))])
        (hash-update attributes attribute (curry cons value) null)))

    `(,(or (dyn:selector-tag selector) '*)
      (,@(for/list ([(name value) (in-hash attributes)])
           `(,name ,(string-join (reverse value) " ")))))))
