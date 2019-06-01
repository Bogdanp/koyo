#lang at-exp racket/base

(require (for-syntax racket/base)
         component
         racket/cmdline
         racket/file
         racket/format
         racket/function
         racket/match
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         raco/command-name
         "runner.rkt")

(define-runtime-path blueprints-path
  (build-path 'up 'up "blueprints"))

(define blueprint-names
  (map path->string (directory-list blueprints-path)))

(define current-program-name
  (make-parameter (short-program+command-name)))

(define (exit-with-errors! . messages)
  (parameterize ([current-output-port (current-error-port)])
    (for-each displayln messages))
  (exit 1))

(define (infer-dynamic-module-path)
  (define files
    (find-files
     (lambda (p)
       (equal? (file-name-from-path p)
               (string->path "dynamic.rkt")))))

  (match files
    [(list f0 f ...) f0]

    [(list)
     (exit-with-errors! "error: could not find a dynamic.rkt module in the current directory")]))

(define (handle-help)
  (exit-with-errors!
   "usage: raco koyo <command> <option> ... <arg> ..."
   ""
   "available commands:"
   "  graph  generate a graph of all the components in the current application"
   "  help   display this help message"
   "  new    generate a new koyo-based project from a blueprint"
   "  serve  run a server for the current application"))

(define (handle-graph)
  (define dynamic-module-path
    (command-line
     #:program (current-program-name)
     #:args ([dynamic-module-path #f])
     (or dynamic-module-path (infer-dynamic-module-path))))

  (define prod-system (dynamic-require dynamic-module-path 'prod-system))
  (define filename (make-temporary-file "system-graph-~a.png"))
  (system->png prod-system filename)
  (void (system* (find-executable-path "open") filename)))

(define (handle-new)
  (define blueprint "standard")
  (define project-name
    (command-line
     #:program (current-program-name)
     #:once-each
     [("-b" "--blueprint") name
                           "the blueprint to use"
                           (if (member name blueprint-names)
                               (set! blueprint name)
                               (exit-with-errors! @~a{error: no blueprint named '@name'}))]
     #:args (name)
     (when (or (directory-exists? name)
               (file-exists? name))
       (exit-with-errors! @~a{error: a file called '@name' already exists in the current directory}))

     name))

  (define root
    (path->complete-path project-name))

  (define project-name/uppercase
    (string-replace (string-upcase project-name) "-" "_"))

  (define project-name/lowercase
    (string-replace (string-downcase project-name) "-" "_"))

  (copy-directory/files
   (build-path blueprints-path blueprint)
   root)

  (rename-file-or-directory
   (build-path root "app-name-here")
   (build-path root project-name))

  (for ([path (find-files (compose not directory-exists?) root)])
    (define contents (file->string path))
    (define replaced-contents
      (regexp-replace*
       #px"AppNameHere|APP_NAME_HERE|app-name-here|app_name_here"
       contents
       (match-lambda
         ["AppNameHere"   project-name]
         ["APP_NAME_HERE" project-name/uppercase]
         ["app-name-here" project-name]
         ["app_name_here" project-name/lowercase])))

    (when (not (string=? contents replaced-contents))
      (call-with-output-file path
        #:mode 'text
        #:exists 'truncate/replace
        (lambda (out)
          (write-string replaced-contents out))))))

(define (handle-serve)
  (define dynamic-module-path
    (command-line
     #:program (current-program-name)
     #:args ([dynamic-module-path #f])
     (or dynamic-module-path (infer-dynamic-module-path))))

  (run-forever (path->complete-path dynamic-module-path)))

(define ((handle-unknown command))
  (exit-with-errors! @~a{error: unrecognized command '@command'}))

(define all-commands
  (hasheq 'graph handle-graph
          'help  handle-help
          'new   handle-new
          'serve handle-serve))

(define-values (command handler args)
  (match (current-command-line-arguments)
    [(vector command args ...)
     (values command (hash-ref all-commands (string->symbol command) (handle-unknown command)) args)]

    [_
     (values "help" handle-help null)]))

(parameterize ([current-command-line-arguments (list->vector args)]
               [current-program-name (~a (current-program-name) " " command)])
  (handler))
