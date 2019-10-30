#lang info

(define collection "koyo")
(define scribblings '(("scribblings/koyo.scrbl" (multi-page))))

(define deps '("base"))
(define build-deps '("component-lib"
                     "koyo-lib"
                     "postmark-client"
                     "sandbox-lib"
                     "scribble-lib"
                     "web-server-lib"

                     "racket-doc"
                     "web-server-doc"))

(define update-implies '("koyo-lib"))
