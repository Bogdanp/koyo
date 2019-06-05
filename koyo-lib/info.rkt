#lang info

(define version "0.1.0")
(define collection 'multi)

(define deps '("base"
               "north"
               "compatibility-lib"
               "component-lib"
               "db-lib"
               "gregor-lib"
               "html-lib"
               "postmark-client"
               "srfi-lite-lib"
               "web-server-lib"))
(define build-deps '("at-exp-lib"))

(define setup-collects '("koyo"))
(define compile-omit-paths '("blueprints"))
(define test-omit-paths '("blueprints"))
