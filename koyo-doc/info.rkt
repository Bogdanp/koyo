#lang info

(define license 'BSD-3-Clause)
(define collection "koyo")
(define scribblings
  '(("scribblings/koyo.scrbl" (multi-page) ("Web Development"))))
(define deps
  '("base"))
(define build-deps
  '("component-doc"
    "component-lib"
    "crontab"
    "db-doc"
    "db-lib"
    "gregor-doc"
    "gregor-lib"
    "koyo-lib"
    "libargon2"
    "mime-type"
    "mime-type-lib"
    "net-doc"
    "net-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"
    "srfi-doc"
    "srfi-lite-lib"
    "web-server-doc"
    "web-server-lib"))
(define update-implies
  '("koyo-lib"))
