#lang racket/base

(require forms
         koyo/continuation
         koyo/database
         koyo/flash
         koyo/haml
         koyo/http
         koyo/l10n
         koyo/url
         racket/contract/base
         racket/match
         racket/string
         threading
         web-server/servlet
         "../components/auth.rkt"
         "../components/mail.rkt"
         "../components/template.rkt"
         "../components/user.rkt"
         "forms.rkt")

;; login & logout ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [login-page (-> auth-manager? (-> request? response?))]
  [logout-page (-> auth-manager? (-> request? response?))]))

(define ((login-page auth) req)
  (define return-url
    (bindings-ref (request-bindings/raw req) 'return (reverse-uri 'dashboard-page)))

  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (define (render render-widget [error-message #f])
         (page
          #:subtitle (translate 'subtitle-log-in)
          (container
           (render-login-form (embed/url loop) render-widget error-message))))

       (match (form-run login-form req)
         [`(passed (,username ,password) ,render-widget)
          (define user-or-message
            (with-handlers ([exn:fail:auth-manager:unverified?
                             (λ (_e) (translate 'error-verify-email))])
              (or (auth-manager-login! auth username password)
                  (translate 'error-invalid-credentials))))
          (match user-or-message
            [(? user?) (redirect-to return-url)]
            [message (render render-widget message)])]

         [`(,_ ,_ ,render-widget)
          (render render-widget)])))))

(define ((logout-page auth) _req)
  (auth-manager-logout! auth)
  (redirect-to (reverse-uri 'login-page)))

(define login-form
  (form* ([username (ensure binding/email (required))]
          [password (ensure binding/text (required))])
    (list username password)))

(define (render-login-form target render-widget [error-message #f])
  (haml
   (:form.form.form--login
    ([:action target]
     [:method "POST"])
    (when error-message
      (haml
       (:ul.form__errors
        (:li error-message))))

    (:h1.form__title (translate 'subtitle-log-in))

    (render-widget "username" (username-field))
    (render-widget "password" (password-field))

    (:button.button.button--primary
     ([:type "submit"])
     (translate 'action-log-in))

    (:a.button.button--secondary
     ([:href (reverse-uri 'signup-page)])
     (translate 'action-sign-up-no-account)))))


;; signup & verify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [signup-page (-> auth-manager? mailer? user-manager? (-> request? response?))]
  [verify-page (-> user-manager? (-> request? integer? string? response?))]))

(define ((signup-page auth mailer users) req)
  (send/suspend/dispatch/protect
   (lambda (embed/url)
     (define (render render-widget [error-message #f])
       (page
        #:subtitle (translate 'subtitle-sign-up)
        (container
         (render-signup-form (embed/url (signup-page auth mailer users)) render-widget error-message))))

     (match (form-run signup-form req)
       [`(passed (,username ,password) ,render-widget)
        (define user-or-message
          (with-handlers ([exn:fail:user-manager:username-taken?
                           (λ (_e) (translate 'error-username-taken))])
            (user-manager-create! users username password)))
        (match user-or-message
          [(? user? u)
           (mailer-send-welcome-email mailer u)
           (post-signup-page (redirect/get/forget))]
          [message
           (render render-widget message)])]

       [`(,_ ,_ ,render-widget)
        (render render-widget)]))))

(define (post-signup-page _req)
  (page
   #:subtitle (translate 'subtitle-signed-up)
   (haml
    (.container
     (:h1 (translate 'subtitle-signed-up))
     (:p (translate 'message-post-sign-up))))))

(define ((verify-page users) _req user-id verification-code)
  (user-manager-verify! users user-id verification-code)
  (flash 'success (translate 'message-email-verified))
  (redirect-to (reverse-uri 'login-page)))

(define signup-form
  (form* ([username (ensure binding/email (required))]
          [password (ensure binding/text (required) (longer-than 7))])
    (list username password)))

(define (render-signup-form target render-widget [error-message #f])
  (haml
   (:form.form.form--signup
    ([:action target]
     [:method "POST"])

    (:h1.form__title (translate 'subtitle-sign-up))

    (when error-message
      (haml
       (:ul.form__errors
        (:li error-message))))

    (render-widget "username" (username-field))
    (render-widget "password" (password-field))

    (:button.button.button--primary
     ([:type "submit"])
     (translate 'action-sign-up))

    (:a.button.button--secondary
     ([:href (reverse-uri 'login-page)])
     (translate 'action-log-in-signed-up)))))


;; password reset ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [request-password-reset-page (-> mailer? user-manager? (-> request? response?))]
  [password-reset-page (-> user-manager? (-> request? id/c non-empty-string? response?))]))

(define ((request-password-reset-page mailer users) req)
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run request-password-reset-form req)
         [`(passed ,username ,_)
          (define-values (user token)
            (user-manager-create-reset-token! users
                                              #:username username
                                              #:ip-address (request-client-ip req)
                                              #:user-agent (or (and~> (request-headers/raw req)
                                                                      (headers-assq* #"user-agent" _)
                                                                      (header-value)
                                                                      (bytes->string/utf-8))
                                                               "Unknown")))

          (when (and user token)
            (mailer-send-password-reset-email mailer user token))

          (flash 'success (translate 'message-password-reset-requested))
          (redirect/get/forget/protect)
          (redirect-to (reverse-uri 'login-page))]

         [`(,_ ,_ ,render-widget)
          (page
           #:subtitle (translate 'subtitle-request-password-reset)
           (haml
            (.container
             (render-request-password-reset-form (embed/url loop) render-widget))))])))))

(define request-password-reset-form
  (form* ([username (ensure binding/email (required))])
    username))

(define (render-request-password-reset-form target render-widget)
  (haml
   (:form.form.form--password-reset
    ([:action target]
     [:method "POST"])

    (:h1.form__title (translate 'subtitle-request-password-reset))

    (render-widget "username" (username-field))

    (:button.button.button--primary
     ([:type "submit"])
     (translate 'action-request-password-reset)))))

(define ((password-reset-page users) req user-id token)
  (let loop ([req req])
    (send/suspend/dispatch/protect
     (lambda (embed/url)
       (match (form-run password-reset-form req)
         [`(passed ,password ,_)
          (define reset?
            (user-manager-reset-password! users
                                          #:user-id user-id
                                          #:token token
                                          #:password password))

          (if reset?
              (flash 'success (translate 'message-password-reset-success))
              (flash 'error (translate 'message-password-reset-error)))

          (redirect-to (reverse-uri 'login-page))]

         [`(,_ ,_ ,render-widget)
          (page
           #:subtitle (translate 'subtitle-reset-password)
           (haml
            (.container
             (render-password-reset-form (embed/url loop) render-widget))))])))))

(define password-reset-form
  (form* ([password (ensure binding/text (required) (longer-than 7))])
    password))

(define (render-password-reset-form target render-widget)
  (haml
   (:form.form.form--password-reset
    ([:action target]
     [:method "POST"])

    (:h1.form__title (translate 'subtitle-reset-password))

    (render-widget "password" (password-field))

    (:button.button.button--primary
     ([:type "submit"])
     (translate 'action-reset-password)))))
