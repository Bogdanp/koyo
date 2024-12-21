#lang racket/base

(require (for-syntax racket/base)
         racket/file
         racket/format
         racket/match
         racket/runtime-path
         racket/string)

(provide
 blueprints-path
 blueprint-names
 install-blueprint)

(define-runtime-path blueprints-path
  (build-path 'up "blueprints"))

(define blueprint-names
  (map path->string (directory-list blueprints-path)))

(define (make-project-name-replacer project-name)
  (define project-name/uppercase (string-replace (string-upcase   project-name) "-" "_"))
  (define project-name/lowercase (string-replace (string-downcase project-name) "-" "_"))
  (lambda (contents)
    (regexp-replace*
     #px"AppNameHere|APP_NAME_HERE|app-name-here|app_name_here"
     contents
     (match-lambda
       ["AppNameHere"   project-name]
       ["APP_NAME_HERE" project-name/uppercase]
       ["app-name-here" project-name]
       ["app_name_here" project-name/lowercase]))))

(define (install-blueprint project-name blueprint)
  (define root (path->complete-path project-name))
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
  (for ([path (in-list (find-files (compose not directory-exists?) root))])
    (define contents (file->string path))
    (define replaced-contents (replace-project-name contents))
    (unless (string=? contents replaced-contents)
      (call-with-output-file path
        #:mode 'text
        #:exists 'truncate/replace
        (lambda (out)
          (write-string replaced-contents out))))))
