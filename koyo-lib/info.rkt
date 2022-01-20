#lang info

(define version "0.12.1")
(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               ("component-lib" #:version "1.0")
               ("crypto-lib" #:version "1.6")
               "db-lib"
               "errortrace-lib"
               "gregor-lib"
               "html-lib"
               "net-lib"
               "raco-invoke"
               "readline-lib"
               "srfi-lite-lib"
               "unix-socket-lib"
               "web-server-lib"))
(define build-deps '("at-exp-lib"))

(define setup-collects '("koyo"))
(define compile-omit-paths '("blueprints"))
(define test-omit-paths '("blueprints"))
