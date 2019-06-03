#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide
 haml)

(module selectors racket/base
  (require racket/match
           racket/string)

  (provide
   selector?
   selector-tag
   selector-attributes

   attribute?
   attribute-name

   html-symbol?
   html-symbol-value)

  (define (selector? s)
    (define s:str (symbol->string s))
    (and (> (string-length s:str) 1)
         (or (string-prefix? s:str ":")
             (string-prefix? s:str "."))))

  (define selector-tag-re
    #px"(:[^#.]+)?(.+)?")

  (define (selector-tag s)
    (match (regexp-match selector-tag-re (symbol->string s))
      [(list _ #f  _) 'div]
      [(list _ tag _) (string->symbol (substring tag 1))]))

  (define (selector-attributes s)
    (match (regexp-match selector-tag-re (symbol->string s))
      [(list _ _ #f       ) null]
      [(list _ _ attrs:str)
       (define-values (classes ids)
         (for/fold ([classes null] [ids null])
                   ([attr (in-list (regexp-match* #rx"[#.][^#.]+" attrs:str))])
           (cond
             [(string-prefix? attr ".")
              (values (cons (substring attr 1) classes) ids)]

             [(string-prefix? attr "#")
              (values classes (cons (substring attr 1) ids))])))

       (when (> (length ids) 1)
         (raise-syntax-error 'selector (format "an element cannot have multiple ids, found '~V'" ids)))

       (let* ([attrs null]
              [attrs (if (not (null? classes))
                         (cons (list 'class (string-join (reverse classes) " ")) attrs)
                         attrs)]
              [attrs (if (not (null? ids))
                         (cons (list 'id (car ids)) attrs)
                         attrs)])
         attrs)]))

  (define attribute-re
    #px"^:([:a-zA-Z0-9_][:a-zA-Z0-9_-]*)$")

  (define (attribute? s)
    (regexp-match? attribute-re (symbol->string s)))

  (define (attribute-name s)
    (match (regexp-match attribute-re (symbol->string s))
      [(list _ value)
       (string->symbol value)]))

  (define (html-symbol? s)
    (define s:str (symbol->string s))
    (and (> (string-length s:str) 1)
         (string-prefix? (symbol->string s) "&")))

  (define (html-symbol-value s)
    (string->symbol (substring (symbol->string s) 1))))

(begin-for-syntax
  (require 'selectors)

  (define-syntax-class selector
    (pattern selector:id
             #:when (selector? (syntax-e #'selector))
             #:with tag (selector-tag (syntax-e #'selector))
             #:with attributes (selector-attributes (syntax-e #'selector))))

  (define-syntax-class attribute
    (pattern attr:id
             #:when (attribute? (syntax-e #'attr))
             #:with name (attribute-name (syntax-e #'attr))))

  (define-syntax-class attribute-definition
    (pattern (attr:attribute)
             #:with name #'attr.name
             #:with value "")

    (pattern (attr:attribute val:str)
             #:with name #'attr.name
             #:with value #'val)

    (pattern (attr:attribute val:expr)
             #:with name #'attr.name
             #:with value #',val))

  (define-syntax-class literal
    (pattern lit:id
             #:with xexpr (if (html-symbol? (syntax-e #'lit))
                              (html-symbol-value (syntax-e #'lit))
                              #',lit))

    (pattern (~or lit:str lit:number)
             #:with xexpr #'lit))

  (define-syntax-class element
    (pattern (sel:selector (attr:attribute-definition ...+) child:element ...)
             #:with xexpr (with-syntax ([attrs
                                         (append
                                          (syntax-e #'sel.attributes)
                                          (syntax-e #'[(attr.name attr.value) ...]))])
                            #'(sel.tag attrs child.xexpr ...)))

    (pattern (sel:selector child:element ...)
             #:with xexpr #'(sel.tag sel.attributes child.xexpr ...))

    (pattern ((~datum unless) condition:expr child:expr ...+)
             #:with xexpr #',@(if (not condition)
                                  (list child ...)
                                  (list)))

    (pattern ((~datum when) condition:expr child:expr ...+)
             #:with xexpr #',@(if condition
                                  (list child ...)
                                  (list)))

    (pattern ((~datum @) e)
             #:with xexpr #',@e)

    (pattern lit:literal
             #:with xexpr #'lit.xexpr)

    (pattern e:expr
             #:with xexpr #',e)))

(define-syntax (haml stx)
  (syntax-parse stx
    [(_ el:element) #'`el.xexpr]))
