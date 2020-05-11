#lang info

(define collection "tests")

(define deps '())
(define build-deps '("base"
                     "at-exp-lib"
                     "component-lib"
                     "db-lib"
                     "gregor-lib"
                     "koyo-lib"
                     "rackunit-lib"
                     "srfi-lite-lib"
                     "web-server-lib"))

(define update-implies '("koyo-lib"))
