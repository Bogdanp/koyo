#lang info

(define license 'BSD-3-Clause)
(define collection "koyo")
(define scribblings '(("scribblings/koyo.scrbl" (multi-page))))
(define deps '("base"))
(define build-deps '("component-doc"
                     "component-lib"
                     "crontab"
                     "db-lib"
                     "gregor-lib"
                     "koyo-lib"
                     "libargon2"
                     "mime-type"
                     "mime-type-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "srfi-lite-lib"
                     "web-server-lib"

                     "db-doc"
                     "gregor-doc"
                     "net-doc"
                     "racket-doc"
                     "srfi-doc"
                     "web-server-doc"))
(define update-implies '("koyo-lib"))
