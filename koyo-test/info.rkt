#lang info

(define collection 'multi)

(define deps '())
(define build-deps '("base"
                     "koyo-lib"
                     "rackunit-lib"))

(define update-implies '("koyo-lib"))
