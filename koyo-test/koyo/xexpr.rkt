#lang racket/base

(require koyo/xexpr
         rackunit)

(provide xexpr-tests)

(define xexpr-tests
  (test-suite
   "xexpr"

   (test-suite
    "html->xexpr"

    (test-case "groups multiple elements into a single toplevel xexpr by default"
      (check-equal? (html->xexpr "<b>1</b><i>2</i>")
                    '((div ()
                           (b () "1")
                           (i () "2")))))

    (test-case "turns html strings into lists of xexprs"
      (check-equal? (html->xexpr "<b>1</b><i>2</i>" values)
                    '((b () "1")
                      (i () "2")))))

   (test-suite
    "xexpr->text"

    (test-case "turns xexprs into text"
      (check-equal? (xexpr->text "Hi") "Hi")
      (check-equal? (xexpr->text '(h1)) "")
      (check-equal? (xexpr->text '(h1 "Hello" "there")) "Hello there")
      (check-equal? (xexpr->text '(h1 ((class "heading")) "Hi")) "Hi")
      (check-equal? (xexpr->text '(div
                                   ([class "container"])
                                   (h1
                                    ([class "title"])
                                    "A" (strong "good") "title.")
                                   (p
                                    "And some content!")))
                    "A good title. And some content!")
      (check-equal? (xexpr->text '(p "Xexprs" 'mdash "good?")) "Xexprs — good?")
      (check-equal? (xexpr->text '(p "Xexprs" 8212 "good?")) "Xexprs — good?")))

   (test-suite
    "xexpr-select"

    (test-case "finds elements by their tag in a path"
      (define tree
        '(div
          (header
           (h1 "First heading"))
          (main
           (div
            (h1 "Second heading")))
          (footer
           (div
            (div
             (h1 "Third heading")
             (h1 "Fourth heading"))))))

      (check-equal?
       (xexpr-select tree h1)
       '((h1 "First heading")
         (h1 "Second heading")
         (h1 "Third heading")
         (h1 "Fourth heading")))

      (check-equal?
       (xexpr-select tree main h1)
       '((h1 "Second heading")))

      (check-equal?
       (xexpr-select tree footer h1)
       '((h1 "Third heading")
         (h1 "Fourth heading"))))

    (test-case "finds elements by their attributes in a path"
      (define tree
        '(div
          (header
           (a ([class "findme"]
               [data-extra ""])))
          (footer
           (a))))

      (check-equal?
       (xexpr-select tree (a [(class "findme")]))
       '((a ([class "findme"] [data-extra ""]))))

      (check-equal?
       (xexpr-select tree "a.findme")
       '((a ([class "findme"] [data-extra ""])))))

    (test-case "finds elements by subsets of their attributes in a path"
      (define tree
        '(div
          (header
           (a ([class "findme or-findme or-findme-instead"]
               [data-extra ""])))
          (footer
           (a))))

      (check-equal?
       (xexpr-select tree (a [(class "findme")]))
       '((a ([class "findme or-findme or-findme-instead"] [data-extra ""]))))

      (check-equal?
       (xexpr-select tree "a.findme")
       '((a ([class "findme or-findme or-findme-instead"] [data-extra ""]))))

      (check-equal?
       (xexpr-select tree "a.or-findme")
       '((a ([class "findme or-findme or-findme-instead"] [data-extra ""]))))

      (check-equal?
       (xexpr-select tree "a.but-not-me")
       '()))

    (test-case "finds elements by their attributes in a path containing a wildcard"
      (define tree
        '(div
          (header
           (a ([class "findme"])))
          (footer
           (h1 ([class "findme"])))))

      (check-equal?
       (xexpr-select tree (* [(class "findme")]))
       '((a ([class "findme"]))
         (h1 ([class "findme"]))))

      (check-equal?
       (xexpr-select tree footer (* [(class "findme")]))
       '((h1 ([class "findme"]))))

      (check-equal?
       (xexpr-select tree "footer .findme")
       '((h1 ([class "findme"]))))

      (test-case "handles nested elements correctly"
        (define tree
          '(itunes:category
            ([text "Business"])
            (itunes:category ([text "Investing"]))))

        (check-equal?
         (xexpr-select tree * itunes:category)
         `((itunes:category ([text "Investing"]))))))

    (test-suite
     "xexpr-select-text"

     (test-case "finds the text inside elements"
       (define tree
         '(div
           (h1 "first")
           (h1 "second")))

       (check-equal?
        (xexpr-select-text tree h1)
        '("first" "second"))))

    (test-suite
     "xexpr-attr-ref"

     (let ([e `(itunes:category ([text "Business"]))])
       (check-false (xexpr-attr-ref e 'foo))
       (check-equal? (xexpr-attr-ref e 'text) "Business")))

    (test-suite
     "xexpr-attr-ref*"

     (let ([e `(div ([class "a"] [class "b"]))])
       (check-equal? (xexpr-attr-ref* e 'foo) null)
       (check-equal? (xexpr-attr-ref* e 'class) '("a" "b")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests xexpr-tests))
