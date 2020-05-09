#lang info

(define collection "koyo")
(define scribblings '(("scribblings/koyo.scrbl" (multi-page))))

(define deps '("base"))
(define build-deps '("component-lib"
                     "db-lib"
                     "koyo-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "web-server-lib"))

(define update-implies '("koyo-lib"))
