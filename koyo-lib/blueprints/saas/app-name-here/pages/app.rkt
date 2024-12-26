#lang racket/base

(require koyo/haml
         racket/contract/base
         web-server/http
         "../components/static.rkt")

(provide
 (contract-out
  [app-page (-> request? response?)]))

(define (app-page _req)
  (response/xexpr
   #:preamble #"<!doctype html>"
   (haml
    (:html
     ([:lang "en-US"])
     (:head
      (:title "AppNameHere")
      (:meta
       ([:charset "utf-8"]))
      (:meta
       ([:content "width=device-width, initial-scale=1"]
        [:name "viewport"]))
      (:link
       ([:href (static-uri "app.css")]
        [:rel "stylesheet"]
        [:type "text/css"]))
      (:script
       ([:defer "defer"]
        [:src (static-uri "app.js")])))
     (:body
      (:div#app))))))
