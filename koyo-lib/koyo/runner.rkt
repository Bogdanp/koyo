#lang racket/base

(require racket/contract
         racket/file
         racket/function
         racket/future
         racket/list
         racket/match
         racket/path
         racket/system)

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

(define (track path handler)
  (define (collect-tracked-files)
    (map simplify-path (find-files track-file? path #:skip-filtered-directory? #t)))

  (define parent-custodian
    (current-custodian))

  (let loop ([tracked-files (collect-tracked-files)])
    (parameterize ([current-custodian (make-custodian parent-custodian)])
      (define new-tracked-files
        (sync
         (handle-evt
          (filesystem-change-evt path)
          (lambda _
            (collect-tracked-files)))

         (handle-evt
          (apply choice-evt (filter-map
                             (lambda (p)
                               (and (file-exists? p)
                                    (handle-evt (filesystem-change-evt p) (const p))))
                             tracked-files))
          (lambda (p)
            (begin0 tracked-files
              (handler p))))))

      (custodian-shutdown-all (current-custodian))
      (loop new-tracked-files))))

(define (watch path handler)
  (thread
   (lambda ()
     (log-watcher-debug "starting watcher for path ~v" (path->string path))
     (track path (lambda (changed-path)
                   (log-watcher-debug "detected change in ~v" (path->string changed-path))
                   (handler changed-path))))))

(define/contract (run-forever dynamic-module-path
                              #:recompile? [recompile? #t]
                              #:errortrace? [errortrace? #t])
  (->* (path-string?)
       (#:recompile? boolean?
        #:errortrace? boolean?) void?)

  (file-stream-buffer-mode (current-output-port) 'line)
  (file-stream-buffer-mode (current-error-port) 'line)

  (define command-args
    (if errortrace?
        (list "-l" "errortrace" "-t" dynamic-module-path)
        (list dynamic-module-path)))

  (define stop #f)
  (define rb (make-barrier))
  (define (run)
    (match-define (list in out pid err control)
      (parameterize ([subprocess-group-enabled #t])
        (apply process*/ports
               (current-output-port)
               (current-input-port)
               (current-error-port)
               (find-executable-path "racket")
               command-args)))

    (log-runner-info "application process started with pid ~a" pid)
    (barrier-close rb)

    (values
     (handle-evt
      (thread
       (lambda ()
         (control 'wait)))
      (lambda (_)
        (control 'status)))
     (lambda ()
       (control 'interrupt)
       (control 'wait)
       (barrier-open rb))))

  (define (make!)
    (system*/exit-code
     (find-executable-path "raco")
     "make"
     "-j" (number->string (processor-count))
     dynamic-module-path))

  (when recompile?
    (log-runner-info "compiling application")
    (make!))

  (log-runner-info "starting application process")
  (watch (simplify-path (build-path dynamic-module-path 'up 'up))
         (lambda (changed-path)
           (when recompile?
             (log-runner-info (format "recompiling because '~a' changed" changed-path))
             (make!))
           (when stop
             (log-runner-info "stopping application process")
             (stop))))

  (let loop ()
    (define-values (stopped-evt stop-process)
      (run))

    (set! stop stop-process)
    (with-handlers ([exn:break?
                     (lambda (_)
                       (stop))])
      (sync/enable-break
       (handle-evt
        stopped-evt
        (lambda (status)
          (when (eq? status 'done-error)
            (log-runner-warning "application process failed; waiting for changes before reloading")
            (barrier-wait rb))

          (log-runner-info "restarting application process")
          (loop)))))))


;; barrier ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-barrier)
  (make-semaphore 0))

(define (barrier-open b)
  (semaphore-post b))

(define (barrier-wait b)
  (semaphore-wait b))

(define (barrier-close b)
  (let loop ()
    (when (semaphore-try-wait? b)
      (loop))))
