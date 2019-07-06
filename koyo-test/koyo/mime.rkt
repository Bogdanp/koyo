#lang racket/base

(require koyo/mime
         rackunit)

(provide
 mime-tests)

(define mime-tests
  (test-suite
   "mime"

   (test-suite
    "path->mime-type"

    (check-equal? (path->mime-type (string->path "foo/bar.html")) #"text/html")
    (check-equal? (path->mime-type (string->path "test.js")) #"application/javascript")
    (check-equal? (path->mime-type (string->path "test.min.css")) #"text/css"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests mime-tests))
