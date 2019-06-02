#lang racket/base

(require koyo/preload
         koyo/profiler
         racket/format
         web-server/http
         xml)

(provide
 page)

(define (page #:subtitle [subtitle #f]
              . content)

  ;; profile-write is called inside a different thread so we have to
  ;; grab the current profile here and then pass it in to ensure that
  ;; the right profile gets rendered.
  (define profile (current-profile))

  (with-timing 'template "page"
    (define page
      `(html
        (head
         (title
          ,(if subtitle
               (~a subtitle " - AppNameHere")
               "AppNameHere")))
        (body
         ,@content)))

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
