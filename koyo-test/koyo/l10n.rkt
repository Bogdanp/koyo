#lang racket/base

(require gregor
         koyo/l10n
         rackunit
         srfi/29)

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
      (check-equal? (language-header->locale "ro, en-GB;q=0.3, en-US;q=0.1, ro-RO;q=0.5") '(ro . ro))))

   (test-suite
    "localize-date"
    (let ([d (date 1996 2 15)])
      (parameterize ([current-language 'ro])
        (check-equal? (localize-date d)
                      "15 February, 1996"))
      (parameterize ([current-language 'de])
        (check-equal? (localize-date d)
                      "15.02.1996"))
      (parameterize ([current-language 'nonsense])
        (check-equal? (localize-date d)
                      "February 15, 1996"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests l10n-tests))
