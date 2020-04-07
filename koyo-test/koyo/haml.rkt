#lang racket/base

(require koyo/haml
         (submod koyo/haml selectors)
         rackunit)

(provide
 haml-tests)

(define (translate s . args)
  s)

(define haml-tests
  (test-suite
   "haml"

   (test-suite
    "selectors"

    (test-suite
     "selector?"

     (test-case "matches valid selectors"
       (check-false (selector? ':))
       (check-false (selector? (string->symbol ".")))
       (check-true (selector? ':div))
       (check-true (selector? ':a))
       (check-true (selector? '.button))))

    (test-suite
     "selector-tag"

     (test-case "extracts a selector's tag"
       (check-equal? (selector-tag ':div) 'div)
       (check-equal? (selector-tag ':a) 'a)
       (check-equal? (selector-tag '.button) 'div)
       (check-equal? (selector-tag '.container) 'div)
       (check-equal? (selector-tag '.container.container--wide) 'div)))

    (test-suite
     "selector-attributes"

     (test-case "infers classes and ids from a selector"
       (check-equal? (selector-attributes ':div) null)
       (check-equal? (selector-attributes '.a.b)
                     '((class "a b")))
       (check-equal? (selector-attributes 'div#some-id.a.b)
                     '((id "some-id")
                       (class "a b")))
       (check-exn
        exn:fail:syntax?
        (lambda _
          (selector-attributes 'div#id-1#id-2)))))

    (test-suite
     "attribute?"

     (test-case "matches valid attributes"
       (check-false (attribute? '.foo))
       (check-false (attribute? 'class))
       (check-false (attribute? ':class.foo))
       (check-true (attribute? ':class))
       (check-true (attribute? ':data-foo))
       (check-true (attribute? ':cdr:license))))

    (test-suite
     "attribute-name"

     (test-case "extracts names from attributes"
       (check-equal? (attribute-name ':class) 'class)
       (check-equal? (attribute-name ':data-value) 'data-value)
       (check-equal? (attribute-name ':cdr:license) 'cdr:license))))

   (test-suite
    "haml"

    (test-case "converts literals to xexprs"
      (check-equal? (haml 1) 1)
      (check-equal? (haml &mdash) 'mdash)
      (check-equal? (haml "test") "test"))

    (test-case "converts bound identifiers to xexprs"
      (define label "hello")
      (check-equal? (haml label) label))

    (test-case "converts elements to xexprs"
      (check-equal?
       (haml (:div "test"))
       '(div () "test"))

      (check-equal?
       (haml (:div (:a "test")))
       '(div () (a () "test")))

      (check-equal?
       (haml (:div
              ([:data-carousel])
              (:div ([:data-carousel-item]) "1")
              (:div ([:data-carousel-item]) "1")))
       '(div
         ([data-carousel ""])
         (div ([data-carousel-item ""]) "1")
         (div ([data-carousel-item ""]) "1")))

      (check-equal?
       (haml
        (.container
         (.hero
          (:h1.hero__title.upcase
           "Hello!"))))
       '(div
         ([class "container"])
         (div
          ([class "hero"])
          (h1
           ([class "hero__title upcase"])
           "Hello!"))))

      (check-equal?
       (haml
        (.container
         (.hero
          (:a.button
           ([:up-target "body"])
           "Shop"))))
       '(div
         ([class "container"])
         (div
          ([class "hero"])
          (a
           ([class "button"]
            [up-target "body"])
           "Shop"))))

      (check-equal?
       (haml
        (:html
         (:head
          (:title "HAML")
          (:link ([:rel "stylesheet"]
                  [:href "/screen.css"])))
         (:body
          (:h1 "Hello!"))))
       '(html
         []
         (head
          []
          (title [] "HAML")
          (link ([rel "stylesheet"]
                 [href "/screen.css"])))
         (body
          []
          (h1 [] "Hello!")))))

    (test-case "elements can contain arbitrary expressions"
      (check-equal?
       (haml
        (:a.button (translate 'shop)))
       `(a
         ([class "button"])
         ,(translate 'shop)))

      (check-equal?
       (haml
        (:a.button
         ([:up-target (string-upcase "body")])
         "Shop"))
       `(a
         ([class "button"]
          [up-target ,(string-upcase "body")])
         "Shop"))

      (check-equal?
       (haml
        (:a
         (if #t
             "Shop"
             "Magazin")))
       `(a () ,(if #t
                   "Shop"
                   "Magazin"))))

    (test-case "elements can contain literal when and unless expressions"
      (check-equal?
       (haml
        (.container
         (when #t
           (haml (.child-1))
           (haml (.child-2)))))
       `(div
         ([class "container"])
         (div ([class "child-1"]))
         (div ([class "child-2"]))))

      (check-equal?
       (haml
        (.container
         (when #f
           (haml (.child-1))
           (haml (.child-2)))))
       `(div
         ([class "container"])))

      (check-equal?
       (haml
        (.container
         (unless #t
           (haml (.child-1))
           (haml (.child-2)))))
       `(div
         ([class "container"])))

      (check-equal?
       (haml
        (.container
         (unless (not #t)
           (haml (.child-1))
           (haml (.child-2)))))
       `(div
         ([class "container"])
         (div ([class "child-1"]))
         (div ([class "child-2"])))))

    (test-case "expressions can be spliced into elements using (@ ...) syntax"
      (check-equal?
       (haml
        (.articles (@ (map symbol->string '(a b c)))))
       '(div
         ([class "articles"])
         "a" "b" "c")))

    (test-case "expressions can be spliced into elements using unquote splicing"
      (check-equal?
       (haml
        (.articles ,@(map symbol->string '(a b c))))
       '(div
         ([class "articles"])
         "a" "b" "c")))

    (test-case "a list of elements produces a list of xexprs"
      (check-equal?
       (haml
        (:li "a")
        (:li "b"))
       '((li () "a")
         (li () "b")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests haml-tests))
