#lang at-exp racket/base

(require component
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/system
         raco/command-name
         raco/invoke
         "blueprint.rkt"
         "console.rkt"
         "deploy.rkt"
         "generator.rkt"
         "logging.rkt"
         "runner.rkt"
         (submod "runner.rkt" private))

(define-logger koyo)

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
  (if (pair? files)
      (car files)
      (exit-with-errors! "error: could not find a dynamic.rkt module in the current directory")))

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
   "  deploy    deploy an application distribution to one or more servers"
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

(define (handle-deploy)
  (define app-name (dirname (current-directory)))
  (define destination #f)
  (define (get-destination)
    (or destination (build-path "/opt" app-name)))
  (define env-var-prefix #f)
  (define environment null)
  (define exec-flags null)
  (define exec-name #f)
  (define exec-user #f)
  (define group "www-data")
  (define health-check? #f)
  (define ports
    (make-hash
     '(["blue"  . 8001]
       ["green" . 8002])))
  (define post-script #f)
  (define pre-script #f)
  (define ssh-flags null)
  (define user app-name)
  (command-line
   #:once-each
   [("--app-name")
    APP_NAME [(format "the name of the app (default: ~a)" app-name)]
    (set! app-name APP_NAME)]
   [("--destination")
    DESTINATION [(format "the absolute path on the target host where the app will be deployed (default: ~a)" (get-destination))]
    (set! destination DESTINATION)]
   [("--env-var-prefix")
    ENV_VAR_PREFIX [(format "the prefix for environment variables (default: ~a)" app-name)]
    (set! env-var-prefix #f)]
   [("--exec-flags")
    EXEC_FLAGS "additional flags to pass to the application executable"
    (set! exec-flags (regexp-split #rx"[ ]" EXEC_FLAGS))]
   [("--exec-name")
    EXEC_NAME [(format "the executable name (default: ~a)" app-name)]
    (set! exec-name EXEC_NAME)]
   [("--exec-user")
    EXEC_USER [(format "the user to run the service as (default: ~a)" user)]
    (set! exec-user EXEC_USER)]
   [("--group")
    GROUP "the group that will own the uploaded files (default: www-data)"
    (set! group GROUP)]
   [("--health-check")
    "turn on health checking"
    (set! health-check? #t)]
   [("--ssh-flags" "-F")
    SSH_FLAGS "additional flags to pass to the SSH command"
    (set! ssh-flags (regexp-split #rx"[ ]" SSH_FLAGS))]
   [("--user")
    USER [(format "the user that should own the uploaded files (default: ~a)" app-name)]
    (set! user USER)]
   #:multi
   [("--environment" "-e")
    KEY VALUE "zero or more environment values to pass to the app at runtime"
    (set! environment (cons (cons KEY VALUE) environment))]
   [("--port" "-p")
    VARIANT PORT "maps PORT to VARIANT (default: \"blue 8001\", \"green 8002\")"
    (unless (member VARIANT '("blue" "green"))
      (exit-with-errors! @~a{error: port KEY must be either 'blue' or 'green'}))
    (define port-num (string->number PORT))
    (unless (and port-num
                 (>= port-num 1)
                 (<= port-num 65535))
      (exit-with-errors! @~a{error: port PORT must be an integer in the range [1, 65535]}))
    (hash-set! ports VARIANT PORT)]
   [("--post-script")
    PATH "a path to a bash script to run after the service is started"
    (set! post-script (call-with-input-file PATH port->string))]
   [("--pre-script")
    PATH "a path to a bash script to run before the service is started"
    (set! pre-script (call-with-input-file PATH port->string))]
   #:args [distribution version . hosts]
   (with-handlers ([exn:fail?
                    (lambda (e)
                      (exit-with-errors! @~a{error: @(exn-message e)}))])
     (deploy
      #:app-name app-name
      #:destination (get-destination)
      #:env-var-prefix (or env-var-prefix app-name)
      #:environment environment
      #:exec-flags exec-flags
      #:exec-name (or exec-name app-name)
      #:exec-user (or exec-user user)
      #:group group
      #:health-check? health-check?
      #:ports ports
      #:post-script post-script
      #:pre-script pre-script
      #:ssh-flags ssh-flags
      #:user user
      distribution version hosts))))

(define (handle-dist)
  (define target-path "dist")
  (define included-langs null)
  (define included-libs null)
  (define dynamic-module-path
    (command-line
     #:program (current-program-name)
     #:once-each
     [("-t" "--target")
      TARGET-PATH "where to put the distribution"
      (set! target-path TARGET-PATH)]
     #:multi
     [("++lang")
      LANG "additional #langs to include in the executable"
      (set! included-langs (cons LANG included-langs))]
     [("++lib")
      LIB "additional libs to include in the executable"
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
      PATH "the path to dynamic.rkt, inferred by default"
      (set! dynamic-module-path PATH)]
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
     [("-b" "--blueprint")
      NAME "the blueprint to use"
      (if (member NAME blueprint-names)
          (set! blueprint NAME)
          (exit-with-errors! @~a{error: no blueprint named '@NAME'}))]
     #:args [name]
     (cond
       [(string=? name ".")
        (exit-with-errors! @~a{error: "." cannot be used as project name})]
       [(not (regexp-match-exact? #rx"[-_a-zA-Z0-9]*" name))
        (exit-with-errors! @~a{error: '@name' cannot be used as project name (invalid collection name)})]
       [(or (directory-exists? name) (file-exists? name))
        (exit-with-errors! @~a{error: a file called '@name' already exists in the current directory})]
       [else
        name])))
  (define root (path->complete-path project-name))
  (install-blueprint root project-name blueprint))

(define (handle-update)
  (define blueprint "standard")
  (define project-name #f)
  (define root-path
    (command-line
     #:program (current-program-name)
     #:once-each
     [("-b" "--blueprint")
      BLUEPRINT "the blueprint to use"
      (if (member BLUEPRINT blueprint-names)
          (set! blueprint BLUEPRINT)
          (exit-with-errors! @~a{error: no blueprint named '@BLUEPRINT'}))]
     [("-n" "--name")
      NAME "the project name"
      (set! project-name NAME)]
     #:args [path]
     (normalize-path (path->complete-path path))))
  (unless project-name
    (set! project-name (path->string (file-name-from-path root-path))))
  (with-handlers ([exn:break? void])
    (update-blueprint root-path project-name blueprint)))

(define (handle-serve)
  (define recompile? #t)
  (define errortrace? #f)
  (define server-timeout 30)
  (define watch-patterns null)
  (define watch-excludes null)
  (define watch-verbose? #f)
  (define dynamic-module-path
    (command-line
     #:program (current-program-name)
     #:multi
     [("--watch-pattern")
      PATTERN-RE "a regular expression to include files & folders in the watched set"
      (set! watch-patterns (cons (regexp PATTERN-RE) watch-patterns))]
     [("--watch-exclude")
      PATTERN-RE "a regular expression to exclude files & folders from the watched set"
      (set! watch-excludes (cons (regexp PATTERN-RE) watch-excludes))]
     #:once-each
     [("--errortrace")
      "run the application with errortrace"
      (set! errortrace? #t)]
     [("--disable-recompile")
      "don't recompile changed files on reload"
      (set! recompile? #f)]
     [("--server-timeout")
      t "server startup timeout in seconds"
      (set! server-timeout (or (string->number t) server-timeout))]
     [("--log-watched-files")
      "log which files get watched according to --watch-pattern and --watch-exclude"
      (set! watch-verbose? #t)]
     #:args ([dynamic-module-path #f])
     (if dynamic-module-path
         (string->path dynamic-module-path)
         (infer-dynamic-module-path))))

  (define watch-file?-proc
    (if (and (null? watch-patterns)
             (null? watch-excludes))
        watch-file?
        (lambda (p)
          (define watch?
            (and
             (ormap (λ (re) (regexp-match? re p)) watch-patterns)
             (not (ormap (λ (re) (regexp-match? re p)) watch-excludes))))
          (begin0 watch?
            (when (and watch? watch-verbose?)
              (let ([t (if (directory-exists? p) "directory" "file")])
                (log-watcher-debug "watching ~a ~a" t p)))))))

  (run-forever
   #:recompile? recompile?
   #:errortrace? errortrace?
   #:server-timeout server-timeout
   #:watch-file?-proc watch-file?-proc
   (path->complete-path dynamic-module-path)))

(define ((handle-unknown command))
  (exit-with-errors! @~a{error: unrecognized command '@command'}))

(define (dirname p)
  (define-values (_base name _must-be-dir?)
    (split-path p))
  name)

(module+ main
  (define stop-logger
    (start-logger
     #:levels
     '((koyo    . debug)
       (runner  . debug)
       (watcher . debug))))

  (define all-commands
    (hasheq 'console  handle-console
            'deploy   handle-deploy
            'dist     handle-dist
            'generate handle-generate
            'graph    handle-graph
            'help     handle-help
            'new      handle-new
            'serve    handle-serve
            'update   handle-update))

  (define-values (command handler args)
    (match (current-command-line-arguments)
      [(vector command args ...) ;; noqa
       (values command (hash-ref all-commands (string->symbol command) (handle-unknown command)) args)]
      [_
       (values "help" handle-help null)]))

  (parameterize ([current-command-line-arguments (list->vector args)]
                 [current-program-name (~a (current-program-name) " " command)])
    (handler)
    (stop-logger)))
