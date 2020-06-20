#lang info

(define collection "koyo")
(define scribblings '(("scribblings/koyo.scrbl" (multi-page))))

(define deps '("base"))
(define build-deps '("component-doc"
                     "component-lib"
                     "db-lib"
                     "gregor-lib"
                     "koyo-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "srfi-lite-lib"
                     "web-server-lib"

                     "db-doc"
                     "gregor-doc"
                     "net-doc"
                     "racket-doc"
                     "srfi-doc-nonfree"
                     "web-server-doc"))

(define update-implies '("koyo-lib"))
