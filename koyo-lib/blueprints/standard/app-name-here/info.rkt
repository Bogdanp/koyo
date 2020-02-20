#lang info

(define collection "app-name-here")
(define deps '("base"
               "component-lib"
               "crypto-lib"
               "db-lib"
               "deta-lib"
               "forms-lib"
               "gregor-lib"
               "koyo-lib"
               "koyo-north"
               "koyo-postmark"
               ("libargon2-x86_64-linux" #:platform #rx"x86_64-linux")
               ("libargon2-x86_64-macosx" #:platform #rx"x86_64-macosx")
               "threading-lib"
               "web-server-lib"))
(define build-deps '())
