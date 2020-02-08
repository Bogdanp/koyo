#lang racket/base

(require racket/contract
         racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/string
         racket/system)

(provide
 run-forever)

(define-logger runner)
(define-logger watcher)

(define (track-file? p)
  (if (directory-exists? p)
      (match/values (split-path p)
        [(_ (app path->string (regexp "^\\.")) _) #f]
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
  (define (run)
    (match-define (list in out pid err control)
      (apply process*/ports
             (current-output-port)
             (current-input-port)
             (current-error-port)
             (find-executable-path "racket")
             command-args))

    (values
     (thread
      (lambda ()
        (control 'wait)))
     (lambda ()
       (control 'interrupt))))

  (define (make!)
    (system*/exit-code (find-executable-path "raco") "make" "-v" dynamic-module-path))

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
    (with-handlers ([exn:break? (lambda _
                                  (stop))])
      (sync/enable-break
       (handle-evt stopped-evt
                   (lambda _
                     (log-runner-info "restarting application process")
                     (loop)))))))
