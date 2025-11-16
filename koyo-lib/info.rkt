#lang info

(define license 'BSD-3-Clause)
(define version "0.47")
(define collection 'multi)

(define deps
  '("actor-lib"
    ["base" #:version "8.1.0.4"]
    "box-extra-lib"
    ["buid-lib" #:version "1.2"]
    ["component-lib" #:version "1.0"]
    ["crontab-lib" #:version "0.2"]
    ["crypto-lib" #:version "1.6"]
    "db-lib"
    "define-query-lib"
    "errortrace-lib"
    "gregor-lib"
    "html-lib"
    "mime-type-lib"
    "monocle-lib"
    "net-lib"
    "raco-invoke"
    ["resource-pool-lib" #:version "0.6"]
    "scribble-text-lib"
    "srfi-lite-lib"
    "struct-define"
    "threading-lib"
    "unix-socket-lib"
    "version-case"
    "web-server-lib"))
(define build-deps
  '("at-exp-lib"))

(define setup-collects '("koyo"))
(define compile-omit-paths '("blueprints"))
(define test-omit-paths '("blueprints"))
