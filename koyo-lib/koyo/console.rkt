#lang racket/base

(require racket/system)

(provide
 start-console)

(define preload-modules
  '(racket component db koyo))

(define preamble
  '(begin
     (module dyn racket/base
       (require (for-syntax racket/base
                            racket/string
                            syntax/parse)
                component)

       (provide
        (rename-out [$%top #%top])
        current-dev-system)

       (define current-dev-system (make-parameter #f))

       (define-syntax ($%top stx)
         (syntax-parse stx
           [(_ . id)
            #:when (string-prefix? (symbol->string (syntax-e #'id)) "$%")
            #:with cid (datum->syntax #'id (string->symbol (let ([e:str (symbol->string (syntax-e #'id))])
                                                             (substring e:str 2 (string-length e:str)))))
            #'(system-ref (current-dev-system) 'cid)]

           [(_ . id)
            #'(#%top . id)])))

     (require readline 'dyn)

     (define dev-system
       (system-replace prod-system 'server values))

     (current-dev-system dev-system)

     (define ($%start) (system-start dev-system))
     (define ($%stop)  (system-stop  dev-system))

     ($%start)))

(define (start-console module-path)
  (displayln "Compiling application...")
  (system*/exit-code (find-executable-path "raco") "make" "-v" module-path)
  (displayln "Starting REPL...")
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (for ([mod (in-list preload-modules)])
      (namespace-require mod))
    (namespace-require module-path)
    (eval preamble)
    (read-eval-print-loop)))
