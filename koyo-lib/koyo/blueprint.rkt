#lang racket/base

(require (for-syntax racket/base)
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

(provide
 blueprints-path
 blueprint-names
 install-blueprint
 update-blueprint)

(define-runtime-path blueprints-path
  (build-path 'up "blueprints"))

(define blueprint-names
  (map path->string (directory-list blueprints-path)))

(define (make-project-name-replacer project-name)
  (define project-name/uc (string-replace (string-upcase   project-name) "-" "_"))
  (define project-name/lc (string-replace (string-downcase project-name) "-" "_"))
  (lambda (contents)
    (regexp-replace*
     #px"AppNameHere|APP_NAME_HERE|app-name-here|app_name_here"
     (if (path? contents) (path->string contents) contents)
     (match-lambda
       ["AppNameHere"   project-name]
       ["APP_NAME_HERE" project-name/uc]
       ["app-name-here" project-name]
       ["app_name_here" project-name/lc]))))

(define (install-blueprint root project-name blueprint)
  (define replace-project-name
    (make-project-name-replacer project-name))
  (copy-directory/files
   (build-path blueprints-path blueprint)
   root)
  (rename-file-or-directory
   (build-path root "app-name-here")
   (build-path root project-name))
  (define project-tests-path
    (build-path root "app-name-here-tests"))
  (when (directory-exists? project-tests-path)
    (rename-file-or-directory
     (build-path project-tests-path "app-name-here")
     (build-path project-tests-path project-name))
    (rename-file-or-directory
     project-tests-path
     (build-path root (~a project-name "-tests"))))
  (for ([path (in-list (find-files (compose1 not directory-exists?) root))])
    (define contents (file->string path))
    (define replaced-contents (replace-project-name contents))
    (unless (string=? contents replaced-contents)
      (call-with-output-file path
        #:mode 'text
        #:exists 'truncate/replace
        (lambda (out)
          (write-string replaced-contents out))))))

(define (update-blueprint root project-name blueprint)
  (define replace-project-name
    (make-project-name-replacer project-name))
  (define blueprint-root
    (normalize-path (build-path blueprints-path blueprint)))
  (for ([src-path (in-directory blueprint-root)])
    (define dst-path
      (replace-project-name
       (reroot-path src-path blueprint-root root)))
    (if (directory-exists? src-path)
        (make-directory* dst-path)
        (with-handlers ([exn:fail:filesystem?
                         (lambda (_)
                           (copy-file src-path dst-path))])
          (define dst-filename (file-name-from-path dst-path))
          (define dst-contents (file->string dst-path))
          (define src-contents (replace-project-name (file->string src-path)))
          (printf "Updating ~a...~n" dst-filename)
          (unless (equal? dst-contents src-contents)
            (printf "  = ~a differs from blueprint~n" dst-filename)
            (call-with-temporary-file
             (lambda (tmp-path)
               (call-with-output-file tmp-path
                 #:exists 'truncate/replace
                 (λ (out) (write-string src-contents out)))
               (define-values (same? patch)
                 (make-patch dst-path tmp-path))
               (unless same?
                 (display patch)
                 (unless (string-suffix? patch "\n")
                   (newline))
                 (let loop ()
                   (match (prompt "Patch ~a? [R/a/n/c/?] " dst-filename)
                     ["n" (void)]
                     ["c" (exit 1)]
                     ["a" (apply-patch dst-path patch)]
                     ["R" (copy-file tmp-path dst-path)]
                     [_ (displayln " Answer nothing or ? for 'help, R for 'replace',")
                        (displayln " a for 'apply', n for 'no', or 'c' for 'cancel'.")
                        (loop)]))))))))))

(define (prompt message . args)
  (apply printf message args)
  (read-line))

(define (reroot-path path src-root dst-root)
  (define path-segments (explode-path path))
  (define src-root-segments (explode-path src-root))
  (define dst-root-segments (explode-path dst-root))
  (define dst-path-segments
    (append
     dst-root-segments
     (drop path-segments (length src-root-segments))))
  (apply build-path dst-path-segments))

(define (call-with-temporary-file proc)
  (define path #f)
  (dynamic-wind
    (λ () (set! path (make-temporary-file)))
    (λ () (proc path))
    (λ () (delete-file path))))

(define (make-patch dst-path src-path)
  (match-define (list stdout stdin _pid stderr control)
    (process* (find-executable-path "git") "diff" "--no-index" "-p" dst-path src-path))
  (control 'wait)
  (define same? (zero? (control 'exit-code)))
  (close-output-port stdin)
  (define output (port->string stdout))
  (close-input-port stdout)
  (close-input-port stderr)
  (values same? output))

(define (apply-patch dst-path patch)
  (match-define (list stdout stdin _pid stderr control)
    (process* (find-executable-path "patch") "-p1" dst-path))
  (write-string patch stdin)
  (close-output-port stdin)
  (control 'wait)
  (define message (port->string stderr))
  (close-input-port stdout)
  (close-input-port stderr)
  (unless (zero? (control 'exit-code))
    (error 'patch "~a" message)))
