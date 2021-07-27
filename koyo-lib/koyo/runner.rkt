#lang racket/base

(require racket/contract
         racket/file
         racket/format
         racket/future
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         "private/tool.rkt")

(provide
 run-forever)

(define-logger runner)
(define-logger watcher)

(define (track-file? p)
  (if (directory-exists? p)
      (match/values (split-path p)
        [(_ (app path->string (regexp "^\\.")) _) #f]
        [(_ (app path->string "compiled")      _) #f]
        [(_ (app path->string "migrations")    _) #f]
        [(_ (app path->string "node_modules")  _) #f]
        [(_ _                                  _) #t])
      (case (path-get-extension p)
        [(#".rkt" #".rktd" #".ss") #t]
        [(#".html" #".sql")        #t]
        [else                      #f])))

(define (collect-tracked-files path)
  (map simplify-path (find-files track-file? path #:skip-filtered-directory? #t)))

(define (code-change-evt root-path)
  (apply
   choice-evt
   (for/list ([p (in-list (collect-tracked-files root-path))] #:when (file-exists? p))
     (define chg (filesystem-change-evt p))
     (nack-guard-evt
      (lambda (nack)
        (thread
         (lambda ()
           (sync nack)
           (filesystem-change-evt-cancel chg)))
        (handle-evt chg (λ (_) p)))))))

(define/contract (run-forever dynamic-module-path
                              #:recompile? [recompile? #t]
                              #:errortrace? [errortrace? #t]
                              #:server-timeout [server-timeout 30])
  (->* (path-string?)
       (#:recompile? boolean?
        #:errortrace? boolean?
        #:server-timeout (and/c real? positive?))
       void?)

  (file-stream-buffer-mode (current-output-port) 'line)
  (file-stream-buffer-mode (current-error-port) 'line)

  (define root-path (simplify-path (build-path dynamic-module-path 'up 'up)))
  (define command-args
    (if errortrace?
        (list "-l" "errortrace" "-l" "koyo/runner" "--" root-path dynamic-module-path)
        (list "-l" "koyo/runner" "--" root-path dynamic-module-path)))

  (define (run)
    (define-values (command-in command-out)
      (make-pipe))
    (define-values (stderr-in stderr-out)
      (make-pipe))
    (match-define (list _in _out pid err control)
      (parameterize ([subprocess-group-enabled #t])
        (apply process*/ports
               (current-output-port)
               command-in
               stderr-out
               racket-exe
               command-args)))

    (define ready? (make-semaphore))
    (define _stderr-filter
      (thread
       (lambda ()
         (parameterize ([current-output-port (current-error-port)])
           (for ([line (in-lines stderr-in)])
             (when (string-contains? line "server: listening")
               (semaphore-post ready?))
             (displayln line))))))

    (define stopped-evt
      (handle-evt
       (thread
        (lambda ()
          (control 'wait)))
       (lambda (_)
         (control 'status))))

    (unless (sync/timeout server-timeout stopped-evt ready?)
      (log-runner-warning "timed out while waiting for 'listening' output"))
    (log-runner-info "application process started with pid ~a" pid)

    (values
     stopped-evt
     (lambda (changed-path)
       (log-runner-info "reloading application because '~a' changed" changed-path)
       (write `(reload ,(path->string changed-path)) command-out)
       (sync/timeout
        server-timeout
        (handle-evt stopped-evt void)
        (handle-evt
         ready?
         (λ (_)
           (log-runner-info "application reloaded")))))
     (lambda ()
       (control 'interrupt)
       (control 'wait)
       (close-output-port stderr-out)
       (close-input-port command-in)
       (close-output-port command-out))))

  (define (maybe-compile-app!)
    (when recompile?
      (log-runner-info "compiling application")
      (unless (make! dynamic-module-path)
        (log-runner-warning "compilation failed"))))

  (define (maybe-recompile-app! changed-path)
    (void
     (thread
      (lambda ()
        (when recompile?
          (log-runner-info "recompiling because '~a' changed" changed-path)
          (parameterize ([current-output-port (open-output-nowhere)]
                         [current-error-port  (open-output-nowhere)])
            (unless (make! dynamic-module-path #:parallel? #f)
              (log-runner-warning "compilation failed (output suppressed)"))))))))

  (let process-loop ()
    (maybe-compile-app!)
    (log-runner-info "starting application process")
    (let-values ([(stopped-evt reload stop) (run)])
      (let filesystem-loop ()
        (with-handlers ([exn:break? (λ (_) (stop))])
          (sync/enable-break
           (handle-evt
            stopped-evt
            (lambda (status)
              (when (eq? status 'done-error)
                (log-runner-warning "application process failed; waiting for changes before reloading")
                (sync (code-change-evt root-path)))
              (process-loop)))
           (handle-evt
            (code-change-evt root-path)
            (lambda (changed-path)
              (reload changed-path)
              (unless (symbol? (sync/timeout 0 stopped-evt))
                ;; Don't recompile the app if it has stopped.  The
                ;; process loop will take care of that.
                (maybe-recompile-app! changed-path))
              (filesystem-loop)))))))))

(module+ main
  (require racket/cmdline
           racket/rerequire
           setup/collects
           (prefix-in jobs: (submod "job/registry.rkt" private))
           "private/mod.rkt"
           "private/zo.rkt")

  (file-stream-buffer-mode (current-output-port) 'line)
  (file-stream-buffer-mode (current-error-port)  'line)

  (define-values (root-path dynamic-module-path)
    (command-line
     #:program "raco koyo serve"
     #:args (root-path dynamic-module-path)
     (values root-path dynamic-module-path)))

  (define mod (path->module-path dynamic-module-path))
  (define (start)
    (jobs:clear!)
    (dynamic-rerequire mod)
    (values
     ((dynamic-require mod 'start))
     (dynamic-require mod 'before-reload (λ () void))))

  (let loop ()
    (let-values ([(stop before-reload) (start)])
      (let inner-loop ()
        (with-handlers ([exn:break? (λ (_) (stop))]
                        [constant-redefined-exn?
                         (lambda (e)
                           (eprintf "koyo/runner: ~a~n" (exn-message e))
                           (delete-zos! root-path)
                           (exit 0))])
          (sync/enable-break
           (handle-evt
            (current-input-port)
            (lambda (_)
              (match (read)
                [`(reload ,changed-path)
                 (stop)
                 (when should-touch-dependents?
                   (touch-dependents dynamic-module-path changed-path))
                 (before-reload)
                 (loop)]

                [message
                 (eprintf "koyo/runner: unhandled message ~e~n" message)
                 (inner-loop)])))))))))
