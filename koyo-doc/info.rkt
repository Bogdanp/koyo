#lang info

(define collection "koyo")
(define scribblings '(("scribblings/koyo.scrbl" ())))

(define deps '("base"))
(define build-deps '("koyo-lib"
                     "sandbox-lib"
                     "scribble-lib"

                     "racket-doc"
                     "web-server-doc"))
(define update-implies '("koyo-lib"))
