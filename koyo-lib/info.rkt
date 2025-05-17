#lang info

(define license 'BSD-3-Clause)
(define version "0.33.1")
(define collection 'multi)

(define deps
  '("actor-lib"
    ["base" #:version "8.1.0.4"]
    ["buid-lib" #:version "1.2"]
    ["component-lib" #:version "1.0"]
    ["crontab-lib" #:version "0.1.1"]
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
    "scribble-text-lib"
    "srfi-lite-lib"
    "struct-define"
    "threading-lib"
    "unix-socket-lib"
    "web-server-lib"))
(define build-deps
  '("at-exp-lib"))

(define setup-collects '("koyo"))
(define compile-omit-paths '("blueprints"))
(define test-omit-paths '("blueprints"))
