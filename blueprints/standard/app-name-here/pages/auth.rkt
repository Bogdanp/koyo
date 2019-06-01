#lang racket/base

(require forms
         koyo/flash
         koyo/l10n
         koyo/url
         racket/contract/base
         racket/match
         threading
         web-server/servlet
         "../components/auth.rkt"
         "../components/mail.rkt"
         "../components/user.rkt"
         "../components/template.rkt")

(provide
 (contract-out
  [login-page (-> auth-manager? (-> request? response?))]
  [logout-page (-> auth-manager? (-> request? response?))]
  [signup-page (-> auth-manager? mailer? user-manager? (-> request? response?))]
  [verify-page (-> flash-manager? user-manager? (-> request? integer? string? response?))]))


(define ((make-labeled-field label name widget) render-widget)
  `(div
    ((class "form__group"))
    (label ,label ,(render-widget name widget))
    ,@(render-widget name (widget-errors #:class "form__errors"))))


;; login & logout ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define login-form
  (form* ([username (ensure binding/email (required))]
          [password (ensure binding/text (required))])
    (list username password)))

(define (render-login-form target render-widget [error-message #f])
  (define render-username-field
    (make-labeled-field (translate 'label-username) "username" (widget-email #:attributes '((placeholder "bruce@waye.co")))))
  (define render-password-field
    (make-labeled-field (translate 'label-password) "password" (widget-password #:attributes '((placeholder "••••••••••••••••")))))

  `(form
    ((action ,target)
     (method "POST")
     (class "form form--login"))

    ,@(xexpr-when error-message
        `(ul
          ((class "form__errors"))
          (li ,error-message)))

    ,(render-username-field render-widget)
    ,(render-password-field render-widget)

    (button ((class "button button--primary")
             (type "submit"))
            ,(translate 'action-log-in))

    (a ((class "button button--secondary")
        (href ,(reverse-uri 'signup-page)))
       ,(translate 'action-sign-up-no-account))))

(define ((login-page auth) req)
  (define return-url
    (and~> (bindings-assq #"return" (request-bindings/raw req))
           (binding:form-value)
           (bytes->string/utf-8)))

  (let loop ([req req])
    (send/suspend/dispatch
     (lambda (embed/url)
       (define (render render-widget [error-message #f])
         (page
          #:subtitle "Log in"
          (container
           (render-login-form (embed/url loop) render-widget error-message))))

       (match (form-run login-form req)
         [(list 'passed (list username password) render-widget)
          (with-handlers ([exn:fail:auth-manager:unverified?
                           (lambda _
                             (render render-widget (translate 'error-verify-email)))])
            (cond
              [(auth-manager-login! auth username password)
               (redirect-to (or return-url (reverse-uri 'dashboard-page)))]

              [else
               (render render-widget (translate 'error-invalid-credentials))]))]

         [(list _ _ render-widget)
          (render render-widget)])))))

(define ((logout-page auth) req)
  (auth-manager-logout! auth)
  (redirect-to (reverse-uri 'login-page)))


;; signup & verify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define signup-form
  (form* ([username (ensure binding/email (required))]
          [password (ensure binding/text (required) (longer-than 7))])
    (list username password)))

(define (render-signup-form target render-widget [error-message #f])
  (define render-username-field
    (make-labeled-field (translate 'label-username) "username" (widget-email #:attributes '((placeholder "bruce@waye.co")))))
  (define render-password-field
    (make-labeled-field (translate 'label-password) "password" (widget-password #:attributes '((placeholder "••••••••••••••••")))))

  `(form
    ((action ,target)
     (method "POST")
     (class "form form--signup"))

    ,@(xexpr-when error-message
        `(ul
          ((class "form__errors"))
          (li ,error-message)))

    ,(render-username-field render-widget)
    ,(render-password-field render-widget)

    (button ((class "button button--primary")
             (type "submit"))
            ,(translate 'action-sign-up))

    (a ((class "button button--secondary")
        (href ,(reverse-uri 'login-page)))
       ,(translate 'action-log-in-signed-up))))

(define ((signup-page auth mailer users) req)
  (send/suspend/dispatch
   (lambda (embed/url)
     (define (render render-widget [error-message #f])
       (page
        #:subtitle "Sign up"
        (container
         (render-signup-form (embed/url (signup-page auth mailer users)) render-widget error-message))))

     (match (form-run signup-form req)
       [(list 'passed (list username password) render-widget)
        (with-handlers ([exn:fail:user-manager:username-taken?
                         (lambda _
                           (render render-widget (translate 'error-username-taken)))])
          (define user (user-manager-create! users username password))
          (mailer-send-welcome-email mailer user)
          (post-signup-page (redirect/get/forget)))]

       [(list _ _ render-widget)
        (render render-widget)]))))

(define (post-signup-page req)
  (page
   #:subtitle "Signed up"
   (container
    '(h1 "You've been signed up")
    '(p "You need to confirm your e-mail address before you can log in."))))

(define ((verify-page flashes users) req user-id verification-code)
  (user-manager-verify users user-id verification-code)
  (flash flashes 'success "You have successfully verified your e-mail address!")
  (redirect-to (reverse-uri 'login-page)))
