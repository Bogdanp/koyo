#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     racket/string
                     syntax/parse)
         racket/string)

(provide
 haml)

(module selectors racket/base
  (require racket/match
           racket/string
           racket/syntax)

  (provide
   selector?
   selector-tag
   selector-attributes

   attribute?
   attribute-name

   html-symbol?
   html-symbol-value)

  (define (selector? stx)
    (define sel (symbol->string (syntax-e stx)))
    (and (> (string-length sel) 1)
         (or (string-prefix? sel ":")
             (string-prefix? sel "."))))

  (define selector-tag-re
    #px"(:[^#.]+)?(.+)?")

  (define (selector-tag stx)
    (match (regexp-match selector-tag-re (symbol->string (syntax-e stx)))
      [(list _ #f  _) (format-id stx #:source stx "div")]
      [(list _ tag _) (format-id stx #:source stx "~a" (substring tag 1))]))

  (define (selector-attributes stx)
    (match (regexp-match selector-tag-re (symbol->string (syntax-e stx)))
      [(list _ _ #f)
       (datum->syntax stx null stx)]

      [(list _ _ attrs:str)
       (define-values (classes ids)
         (for/fold ([classes null] [ids null])
                   ([attr (in-list (regexp-match* #rx"[#.][^#.]+" attrs:str))])
           (cond
             [(string-prefix? attr ".")
              (values (cons (substring attr 1) classes) ids)]

             [(string-prefix? attr "#")
              (values classes (cons (substring attr 1) ids))]

             [else
              (error 'unreachable)])))

       (when (> (length ids) 1)
         (raise-syntax-error 'selector "an element cannot have multiple ids" stx))

       (define attrs
         (let* ([attrs null]
                [attrs (if (not (null? classes))
                           (cons
                            (with-syntax ([attr (format-id stx #:source stx "class")]
                                          [value (datum->syntax stx (string-join (reverse classes) " ") stx)])
                              (syntax/loc stx
                                [attr value]))
                            attrs)
                           attrs)]
                [attrs (if (not (null? ids))
                           (cons
                            (with-syntax ([attr (format-id stx #:source stx "id")]
                                          [value (datum->syntax stx (car ids) stx)])
                              (syntax/loc stx
                                [attr value]))
                            attrs)
                           attrs)])
           attrs))

       (datum->syntax stx attrs)]))

  (define attribute-re
    #px"^:([:a-zA-Z0-9_@][:a-zA-Z0-9_-]*)$")

  (define (attribute? stx)
    (regexp-match? attribute-re (symbol->string (syntax-e stx))))

  (define (attribute-name stx)
    (match (regexp-match attribute-re (symbol->string (syntax-e stx)))
      [(list _ value)
       (format-id stx #:source stx "~a" value)]))

  (define (html-symbol? stx)
    (define str (symbol->string (syntax-e stx)))
    (and (> (string-length str) 1)
         (string-prefix? str "&")))

  (define (html-symbol-value stx)
    (string->symbol (substring (symbol->string (syntax-e stx)) 1))))

(begin-for-syntax
  (require 'selectors)

  (define (concat-attributes attr-stxes)
    (define attrs&stxes
      (for/fold ([attrs&stxes (hasheq)])
                ([attr-stx (in-list attr-stxes)])
        (syntax-parse attr-stx
          [(name:id e:expr)
           (hash-update attrs&stxes (syntax-e #'name) (λ (es) (cons #'e es)) null)])))
    (for*/fold ([stxes null]
                [seen  null]
                #:result (reverse stxes))
               ([attr-stx (in-list attr-stxes)]
                [attr-id (in-value
                          (syntax-parse attr-stx
                            [(name:id _:expr) (syntax-e #'name)]))]
                #:unless (memq attr-id seen))
      (define stx
        (match (hash-ref attrs&stxes attr-id)
          [`(,_) attr-stx]
          [`(,es ...)
           (syntax-parse attr-stx
             [(attr:id _)
              #:with (e ...) (reverse es)
              (syntax/loc attr-stx
                [attr ,(string-join `(e ...) " ")])])]))
      (values
       (cons stx stxes)
       (cons attr-id seen))))

  (define-syntax-class selector
    (pattern selector:id
             #:when (selector? #'selector)
             #:with tag (selector-tag #'selector)
             #:with attributes (selector-attributes #'selector)))

  (define-syntax-class attribute
    (pattern attr:id
             #:when (attribute? #'attr)
             #:with name (attribute-name #'attr)))

  (define-syntax-class attribute-definition
    (pattern (attr:attribute)
             #:with name #'attr.name
             #:with value #'"")

    (pattern (attr:attribute val:str)
             #:with name #'attr.name
             #:with value #'val)

    (pattern (attr:attribute val:expr)
             #:with name #'attr.name
             #:with value #',val))

  (define-syntax-class literal
    (pattern lit:id
             #:with xexpr (if (html-symbol? #'lit)
                              (html-symbol-value #'lit)
                              #',lit))

    (pattern (~or lit:str lit:number)
             #:with xexpr #'lit))

  (define-syntax-class element
    #:literals (unless unquote-splicing when)
    (pattern (sel:selector (attr:attribute-definition ...+) child:element ...)
             #:with xexpr (with-syntax ([(attr ...)
                                         (concat-attributes
                                          (append
                                           (syntax-e #'sel.attributes)
                                           (syntax-e #'[(attr.name attr.value) ...])))])
                            #'(sel.tag (attr ...) child.xexpr ...)))

    (pattern (sel:selector child:element ...)
             #:with xexpr #'(sel.tag sel.attributes child.xexpr ...))

    (pattern (unless condition:expr child:expr ...+)
             #:with xexpr #',@(if (not condition)
                                  (list child ...)
                                  (list)))

    (pattern (when condition:expr child:expr ...+)
             #:with xexpr #',@(if condition
                                  (list child ...)
                                  (list)))

    (pattern (unquote-splicing e)
             #:with xexpr #',@e)

    (pattern lit:literal
             #:with xexpr #'lit.xexpr)

    (pattern e:expr
             #:with xexpr #',e)))

(define-syntax (haml stx)
  (syntax-parse stx
    #:literals (unquote-splicing)
    [(_ (unquote-splicing el)) #'el]
    [(_ el:element) #'`el.xexpr]
    [(_ el ...+)
     #:with (lst ...)
     (for/list ([stx (in-list (syntax-e #'(el ...)))])
       (syntax-parse stx
         #:literals (unquote-splicing)
         [(unquote-splicing el) #'el]
         [el #'(list (haml el))]))
     (syntax/loc stx
       (append lst ...))]))
