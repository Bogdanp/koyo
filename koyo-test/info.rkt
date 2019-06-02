#lang info

(define collection 'multi)

(define deps '())
(define build-deps '("base"
                     "component-lib"
                     "db-lib"
                     "gregor-lib"
                     "koyo-lib"
                     "rackunit-lib"
                     "web-server-lib"))

(define update-implies '("koyo-lib"))
