#lang info

(define license 'BSD-3-Clause)
(define version "0.23")
(define collection 'multi)

(define deps
  '("base"
    "compatibility-lib"
    ["component-lib" #:version "1.0"]
    "crontab-lib"
    ["crypto-lib" #:version "1.6"]
    "db-lib"
    "errortrace-lib"
    "gregor-lib"
    "html-lib"
    "mime-type-lib"
    "net-lib"
    "raco-invoke"
    "srfi-lite-lib"
    "unix-socket-lib"
    "web-server-lib"))
(define build-deps
  '("at-exp-lib"))

(define setup-collects '("koyo"))
(define compile-omit-paths '("blueprints"))
(define test-omit-paths '("blueprints"))
