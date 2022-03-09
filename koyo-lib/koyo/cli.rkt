#lang at-exp racket/base

(require (for-syntax racket/base)
         component
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         raco/command-name
         raco/invoke
         "console.rkt"
         "generator.rkt"
         "logging.rkt"
         "runner.rkt")

(define-logger koyo)

(define-runtime-path blueprints-path
  (build-path 'up "blueprints"))

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

(define (infer-project-name dmp)
  (path->string
   (last
    (explode-path
     (simplify-path
      (build-path dmp 'up))))))

(define (handle-help)
  (exit-with-errors!
   "usage: raco koyo <command> <option> ... <arg> ..."
   ""
   "available commands:"
   "  console   run a REPL for the current application"
   "  dist      generate a deployable distribution of the application"
   "  generate  generate various types of configuration files"
   "  graph     generate a graph of all the components in the current application"
   "  help      display this help message"
   "  new       generate a new koyo-based project from a blueprint"
   "  serve     run a server for the current application"))

(define (handle-console)
  (start-console
   (command-line
    #:program (current-program-name)
    #:args ([dynamic-module-path #f])
    (or dynamic-module-path (infer-dynamic-module-path)))))

(define (handle-dist)
  (define target-path "dist")
  (define included-langs null)
  (define included-libs null)
  (define dynamic-module-path
    (command-line
     #:program (current-program-name)
     #:once-each
     [("-t" "--target") TARGET-PATH
                        "where to put the distribution"
                        (set! target-path TARGET-PATH)]
     #:multi
     [("++lang") LANG
                 "additional #langs to include in the executable"
                 (set! included-langs (cons LANG included-langs))]
     [("++lib") LIB
                "additional libs to include in the executable"
                (set! included-libs (cons LIB included-libs))]
     #:args ([dynamic-module-path #f])
     (or dynamic-module-path (infer-dynamic-module-path))))

  (define project-name
    (infer-project-name dynamic-module-path))

  (define project-root
    (simplify-path (build-path dynamic-module-path 'up 'up)))

  (define temp-path
    (make-temporary-file "koyo-build~a" 'directory))

  (define temp-exe-path
    (build-path temp-path project-name))

  (log-koyo-info "building executable")
  (unless (zero?
           (apply raco
                  "exe"
                  "-o" (~a temp-exe-path)
                  (append
                   (flatten (for/list ([l (in-list included-langs)]) (list "++lang" l)))
                   (flatten (for/list ([l (in-list included-libs)]) (list "++lib" l)))
                   (list (~a dynamic-module-path)))))
    (exit-with-errors! @~a{error: failed to build racket executable from application}))

  (log-koyo-info "creating distribution")
  (unless (zero? (raco "distribute" (~a target-path) (~a temp-exe-path)))
    (exit-with-errors! @~a{error: failed to create distribution from application}))

  (define static-path
    (build-path project-root "static"))

  (when (directory-exists? static-path)
    (log-koyo-info "copying static folder into target folder")
    (define target-static-path
      (build-path target-path "static"))

    (delete-directory/files target-static-path #:must-exist? #f)
    (copy-directory/files static-path target-static-path)))

(define (handle-generate)
  (define dynamic-module-path (infer-dynamic-module-path))
  (define what
    (command-line
     #:once-each
     [("--dynamic-module-path" "-p")
      path
      "the path to dynamic.rkt, inferred by default"
      (set! dynamic-module-path path)]
     #:args (what)
     what))

  (define project-name
    (infer-project-name dynamic-module-path))

  (define project-root
    (simplify-path (build-path dynamic-module-path 'up 'up)))

  (case what
    [("dockerfile")
     (with-handlers ([exn:fail? (lambda (e)
                                  (exit-with-errors! @~a{error: @(exn-message e)}))])
       (generate-dockerfile! project-root project-name))]
    [else
     (exit-with-errors! @~a{error: unknown template '@|what|'})]))

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
     (cond
       [(string=? name ".")
        (exit-with-errors! @~a{error: "." cannot be used as project name})]
       [(not (regexp-match-exact? #rx"[-_a-zA-Z0-9]*" name))
        (exit-with-errors! @~a{error: '@name' cannot be used as project name (invalid collection name)})]
       [(or (directory-exists? name) (file-exists? name))
        (exit-with-errors! @~a{error: a file called '@name' already exists in the current directory})])

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

  (define project-tests-path (build-path root "app-name-here-tests"))
  (when (directory-exists? project-tests-path)
    (rename-file-or-directory
     (build-path project-tests-path "app-name-here")
     (build-path project-tests-path project-name))
    (rename-file-or-directory
     project-tests-path
     (build-path root (~a project-name "-tests"))))

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
  (define recompile? #t)
  (define errortrace? #f)
  (define server-timeout 30)
  (define dynamic-module-path
    (command-line
     #:program (current-program-name)
     #:once-each
     [("--errortrace") "run the application with errortrace"
                       (set! errortrace? #t)]
     [("--disable-recompile") "don't recompile changed files on reload"
                              (set! recompile? #f)]
     [("--server-timeout") t "server startup timeout in seconds"
                           (set! server-timeout (or (string->number t) server-timeout))]
     #:args ([dynamic-module-path #f])
     (if dynamic-module-path
         (string->path dynamic-module-path)
         (infer-dynamic-module-path))))

  (run-forever
   #:recompile? recompile?
   #:errortrace? errortrace?
   #:server-timeout server-timeout
   (path->complete-path dynamic-module-path)))

(define ((handle-unknown command))
  (exit-with-errors! @~a{error: unrecognized command '@command'}))

;; TODO: Make it possible to control the verbosity?
(define stop-logger
  (start-logger #:levels '((koyo    . debug)
                           (runner  . debug)
                           (watcher . debug))))

(define all-commands
  (hasheq 'console  handle-console
          'dist     handle-dist
          'generate handle-generate
          'graph    handle-graph
          'help     handle-help
          'new      handle-new
          'serve    handle-serve))

(define-values (command handler args)
  (match (current-command-line-arguments)
    [(vector command args ...)
     (values command (hash-ref all-commands (string->symbol command) (handle-unknown command)) args)]

    [_
     (values "help" handle-help null)]))

(parameterize ([current-command-line-arguments (list->vector args)]
               [current-program-name (~a (current-program-name) " " command)])
  (handler)
  (stop-logger))
