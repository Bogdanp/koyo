#lang info

(define collection "app-name-here")
(define deps '("base"
               "component-lib"
               "db-lib"
               ("deta-lib" #:version "0.9")
               "forms-lib"
               "gregor-lib"
               "koyo-lib"
               "koyo-north"
               "koyo-postmark"
               "libargon2"
               "threading-lib"
               "web-server-lib"))
(define build-deps '())
