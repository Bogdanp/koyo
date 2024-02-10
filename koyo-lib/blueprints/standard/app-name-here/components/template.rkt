#lang racket/base

(require koyo/flash
         koyo/haml
         koyo/l10n
         koyo/preload
         koyo/profiler
         koyo/url
         racket/format
         web-server/http
         xml
         (prefix-in config: "../config.rkt")
         "auth.rkt")

(provide
 static-uri
 container
 page)

(define (static-uri path)
  (define path/full (format "/static/~a?rev=~a" path config:version))
  (begin0 path/full
    (track-preload-dependency! path/full)))

(define (container . content)
  (haml (.container ,@content)))

(define (nav . items)
  (haml
   (.nav
    ([:up-nav ""])
    (container
     (haml (.nav__items ,@items))))))

(define (nav-item uri label)
  (haml
   (:li.nav__item
    (:a
     ([:href uri]
      [:up-alias uri])
     label))))

(define (page #:subtitle [subtitle #f]
              #:show-nav? [show-nav? #t]
              . content)

  ;; profile-write is called inside a different thread so we have to
  ;; grab the current profile here and then pass it in to ensure that
  ;; the right profile gets rendered.
  (define profile (current-profile))

  (with-timing 'template "page"
    (define page
      (haml
       (:html
        (:head
         (:meta ([:charset "utf-8"]))
         (:meta ([:name "viewport"] [:content "width=device-width, initial-scale=1"]))

         (:title (if subtitle (~a subtitle " - AppNameHere") "AppNameHere"))
         (:link ([:rel "stylesheet"] [:href (static-uri "app.css")]))

         (:script ([:src (static-uri "app.js")] [:defer "defer"])))
        (:body
         (when show-nav?
           (if (current-user)
               (nav (nav-item (reverse-uri 'dashboard-page) (translate 'nav-dashboard))
                    (nav-item (reverse-uri 'logout-page) (translate 'nav-log-out)))
               (nav (nav-item (reverse-uri 'dashboard-page) (translate 'nav-dashboard))
                    (nav-item (reverse-uri 'login-page) (translate 'nav-log-in))
                    (nav-item (reverse-uri 'signup-page) (translate 'nav-sign-up)))))

         (unless (null? (current-flash-messages))
           (container
            (haml
             (:ul.flash
              ,@(for/list ([flash (current-flash-messages)])
                  (haml
                   (:li
                    ([:class (format "flash__item flash__item--~a" (car flash))])
                    (cdr flash))))))))

         (.content ,@content)))))

    (response
     200
     #"OK"
     (current-seconds)
     #"text/html; charset=utf-8"
     (make-preload-headers)
     (lambda (out)
       (parameterize ([current-output-port out])
         (displayln "<!doctype html>")
         (write-xml/content (xexpr->xml page))
         (profile-write profile))))))
