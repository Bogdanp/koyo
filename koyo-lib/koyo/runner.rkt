#lang racket/base

(require racket/contract
         racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/system)

(provide
 run-forever)

(define-logger runner)
(define-logger watcher)

(define (track-file? p)
  (equal? (path-get-extension p) #".rkt"))

(define (track path handler)
  (define (collect-tracked-files)
    (map simplify-path (find-files track-file? path)))

  (let loop ([tracked-files (collect-tracked-files)])
    (parameterize ([current-custodian (make-custodian)])
      (sync
       (handle-evt
        (filesystem-change-evt path)
        (lambda (e)
          (loop (collect-tracked-files))))

       (handle-evt
        (apply choice-evt (filter-map
                           (lambda (p)
                             (and (file-exists? p)
                                  (handle-evt (filesystem-change-evt p) (const p))))
                           tracked-files))
        (lambda (p)
          (handler p)
          (loop tracked-files)))))))

(define (watch #:path path
               #:handler handler)
  (thread
   (lambda ()
     (log-watcher-debug "starting watcher for path ~v" (path->string path))
     (track path (lambda (changed-path)
                   (log-watcher-debug "detected change in ~v" (path->string changed-path))
                   (handler changed-path))))))

(define/contract (run-forever dynamic-module-path)
  (-> path-string? void?)

  (file-stream-buffer-mode (current-output-port) 'line)
  (file-stream-buffer-mode (current-error-port) 'line)

  (define stop #f)
  (define (run)
    (match-define (list in out pid err control)
      (process*/ports (current-output-port)
                      (current-input-port)
                      (current-error-port)
                      (find-executable-path "racket")
                      "-l" "errortrace" "-t"
                      dynamic-module-path))

    (values
     (thread
      (lambda ()
        (control 'wait)))
     (lambda ()
       (control 'interrupt))))

  (log-runner-info "starting application process")
  (watch
   #:path (simplify-path (build-path dynamic-module-path 'up))
   #:handler (lambda (changed-path)
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
