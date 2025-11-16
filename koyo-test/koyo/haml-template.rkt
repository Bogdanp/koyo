#lang racket/base

(require koyo/haml-template
         rackunit
         syntax/macro-testing)

(provide
 haml-template-tests)

(define haml-template-tests
  (test-suite
   "haml-template"

   (test-case "define"
     (define-haml-template container
       (.container
        (slot)))

     (check-equal?
      (container "hello")
      '(div ([class "container"]) "hello"))
     (check-equal?
      (container "a" "b")
      '(div ([class "container"]) "a" "b"))
     (check-equal?
      (container
       (:article
        (:h1 "Title")))
      '(div
        ([class "container"])
        (article () (h1 () "Title"))))

     (test-case "mix"
       (define-haml-template nav
         (.nav
          ([:up-nav ""])
          (container
           (.nav__items
            (slot)))))

       (check-equal?
        (nav "test")
        '(div
          ([class "nav"]
           [up-nav ""])
          (div
           ([class "container"])
           (div
            ([class "nav__items"])
            "test")))))

     (test-case "no slot"
       (define-haml-template spacer
         (.spacer))
       (check-equal?
        (spacer)
        '(div ([class "spacer"]))))

     (test-case "slots without default"
       (define-haml-template nav-item
         (:li.nav-item
          (:a
           ([:href (slot #:destination)])
           (slot))))

       (check-equal?
        (nav-item #:destination "/" "Home")
        '(li
          ([class "nav-item"])
          (a
           ([href "/"])
           "Home")))

       (check-exn
        #rx"required keyword argument not supplied"
        (lambda ()
          (convert-compile-time-error
           (nav-item "Home")))))

     (test-case "slots with default"
       (define-haml-template nav-item
         (:li.nav-item
          (:a
           ([:href (slot #:destination "/")])
           (slot))))

       (check-equal?
        (nav-item "Home")
        '(li
          ([class "nav-item"])
          (a
           ([href "/"])
           "Home")))
       (check-equal?
        (nav-item #:destination "/account" "My Account")
        `(li
          ([class "nav-item"])
          (a
           ([href "/account"])
           "My Account")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests haml-template-tests))
