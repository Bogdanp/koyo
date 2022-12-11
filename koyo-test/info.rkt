#lang info

(define license 'BSD-3-Clause)
(define collection "tests")

(define deps '())
(define build-deps '("base"
                     "at-exp-lib"
                     "component-lib"
                     "db-lib"
                     "gregor-lib"
                     "koyo-lib"
                     "libargon2"
                     "rackunit-lib"
                     "srfi-lite-lib"
                     "web-server-lib"))

(define update-implies '("koyo-lib"))
