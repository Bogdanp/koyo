#lang info

(define test-omit-paths '("cli.rkt"))
(define raco-commands '(("koyo" (submod koyo/cli main) "run koyo scripts" #f)))
