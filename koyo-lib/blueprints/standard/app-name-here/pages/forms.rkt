#lang racket/base

(require forms
         koyo/haml
         koyo/l10n
         racket/function)

(provide
 field-group
 username-field
 password-field)

(define ((field-group label widget) name value errors)
  (haml
   (.form__group
    (:label label (widget name value errors))
    ,@((widget-errors #:class "form__errors") name value errors))))

(define (username-field [label (translate 'label-username)]
                        [placeholder "you@example.com"])
  (field-group label (curry widget-email #:attributes `((placeholder ,placeholder)))))

(define (password-field [label (translate 'label-password)])
  (field-group label (curry widget-password #:attributes '((placeholder "••••••••••••••••")))))
