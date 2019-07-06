#lang racket/base

(require koyo/l10n
         rackunit)

(provide
 l10n-tests)

(define l10n-tests

  (test-suite
   "l10n"

   (test-suite
    "language-header->locale"

    (parameterize ([current-locales '((en . us)
                                      (ro . ro))])
      (check-equal? (language-header->locale "") #f)
      (check-equal? (language-header->locale "en") #f)
      (check-equal? (language-header->locale "en-US") '(en . us))
      (check-equal? (language-header->locale "en, en-US;q=0.5, ro-RO;q=1") '(ro . ro))
      (check-equal? (language-header->locale "ro, en-GB;q=0.3, en-US;q=0.1, ro-RO;q=0.5") '(ro . ro))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests l10n-tests))
