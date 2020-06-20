#lang info

(define version "0.5.2")
(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               "component-lib"
               "crypto-lib"
               "db-lib"
               "errortrace-lib"
               "gregor-lib"
               "html-lib"
               "readline-lib"
               "srfi-lite-lib"
               "web-server-lib"))
(define build-deps '("at-exp-lib"))

(define setup-collects '("koyo"))
(define compile-omit-paths '("blueprints"))
(define test-omit-paths '("blueprints"))
