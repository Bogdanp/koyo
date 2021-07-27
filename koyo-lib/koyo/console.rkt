#lang racket/base

(require "private/filesystem.rkt"
         "private/tool.rkt")

(provide
 start-console
 start-console-here)

(define preload-modules
  '(racket readline component db koyo))

(define preamble
  '(begin
     (define dev-system
       (system-replace prod-system 'server values))

     (current-system dev-system)

     (define (start) (system-start dev-system))
     (define (stop)  (system-stop  dev-system))

     (start)))

(define (start-console dynamic-module-path)
  (displayln "Compiling application...")
  (make! dynamic-module-path)
  (displayln "Starting REPL...")
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (for ([mod (in-list preload-modules)])
      (namespace-require mod))
    (namespace-require dynamic-module-path)
    (eval preamble)
    (read-eval-print-loop)))

(define (start-console-here)
  (define dynamic-module-path (find-file-in-project "dynamic.rkt" (current-directory)))
  (unless dynamic-module-path
    (raise-user-error 'koyo/console/dev "could not reach dynamic.rkt from here"))
  (for ([mod (in-list preload-modules)])
    (namespace-require mod))
  (namespace-require dynamic-module-path)
  (eval preamble))

(module+ dev
  (start-console-here))
