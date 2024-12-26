#lang info

(define collection "tests")
(define deps '())
(define build-deps
  '("app-name-here"
    "base"
    "component-lib"
    "db-lib"
    "koyo-lib"
    "koyo-north"
    "rackunit-lib"
    "threading-lib"))
(define update-implies
  '("app-name-here"))
