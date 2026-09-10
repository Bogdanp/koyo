#lang racket/base

(require component
         koyo/mail
         rackunit)

(provide
 mail-tests)

(define adapter
  (make-stub-mail-adapter))

(define mailer
  (component-start
   ((make-mailer-factory
     #:adapter adapter
     #:sender "example@koyoweb.org"
     #:common-variables (hasheq 'support-email "support@koyoweb.org")))))

(define mail-tests
  (test-suite
   "mail"

   (test-suite
    "mailer-send-email"

    (test-case "can send text email"
      (mailer-send-email
       #:to "bogdan@example.com"
       #:subject "Hi!"
       #:text-content "Hello!"
       mailer)
      (check-equal?
       (car (stub-mail-adapter-outbox adapter))
       (hasheq 'to "bogdan@example.com"
               'from "example@koyoweb.org"
               'subject "Hi!"
               'text-content "Hello!"
               'html-content #f)))

    (test-case "can send html email"
      (mailer-send-email
       #:to "bogdan@example.com"
       #:subject "Hi!"
       #:html-content "Hello!"
       mailer)
      (check-equal?
       (car (stub-mail-adapter-outbox adapter))
       (hasheq 'to "bogdan@example.com"
               'from "example@koyoweb.org"
               'subject "Hi!"
               'text-content #f
               'html-content "Hello!"))))

   (test-suite
    "mailer-send-email-with-template"

    (test-case "can send email"
      (mailer-send-email-with-template
       #:to "bogdan@example.com"
       #:template-alias 'hello
       #:template-model (hasheq 'message "Hi!")
       mailer)
      (check-equal?
       (car (stub-mail-adapter-outbox adapter))
       (hasheq 'to "bogdan@example.com"
               'from "example@koyoweb.org"
               'template 'hello
               'template-model (hasheq 'message "Hi!"
                                       'support-email "support@koyoweb.org"))))

    (test-case "template models override common variables"
      (mailer-send-email-with-template
       #:to "bogdan@example.com"
       #:template-alias 'hello
       #:template-model (hasheq 'message "Hi!"
                                'support-email "bogdan@koyoweb.org")
       mailer)
      (check-equal?
       (car (stub-mail-adapter-outbox adapter))
       (hasheq 'to "bogdan@example.com"
               'from "example@koyoweb.org"
               'template 'hello
               'template-model (hasheq 'message "Hi!"
                                       'support-email "bogdan@koyoweb.org")))))

   (test-suite
    "mailer-send-email-with-template+extras"

    (test-case "can send email"
      (define extras (gensym 'extras))
      (mailer-send-email-with-template+extras
       mailer extras
       #:to "bogdan@example.com"
       #:template-alias 'hello
       #:template-model (hasheq 'message "Hi!"))
      (check-equal?
       (car (stub-mail-adapter-outbox adapter))
       (hasheq 'to "bogdan@example.com"
               'from "example@koyoweb.org"
               'template 'hello
               'template-model (hasheq 'message "Hi!"
                                       'support-email "support@koyoweb.org")
               'extras extras))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests mail-tests))
