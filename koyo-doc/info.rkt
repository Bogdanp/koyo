#lang info

(define collection "koyo")
(define scribblings '(("scribblings/koyo.scrbl" (multi-page))))

(define deps '("base"))
(define build-deps '("component-doc"
                     "component-lib"
                     "db-lib"
                     "koyo-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "web-server-lib"

                     "db-doc"
                     "net-doc"
                     "racket-doc"
                     "web-server-doc"))

(define update-implies '("koyo-lib"))
