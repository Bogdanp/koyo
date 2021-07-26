#lang racket/base

(require racket/file
         racket/path)

;; If a module was compiled with enforcement of module constants (the
;; default), it will fail to reload.  This module provides utilities
;; for transparently recovering from those cases.

(provide
 constant-redefined-exn?
 delete-zos!)

(define (constant-redefined-exn? e)
  (and (exn:fail:contract:variable? e)
       (regexp-match? #rx"cannot re-define a constant" (exn-message e))))

(define (delete-zos! root)
  (for ([p (in-list (find-files zo-path? root))])
    (eprintf "  [deleting ~a]~n" p)
    (delete-file p)))

(define (zo-path? p)
  (define-values (parent name _)
    (split-path p))
  (and (equal? (path-get-extension name) #".zo")
       (path? parent)
       (zo-dir? parent)))

(define (zo-dir? p)
  (define expected
    (let ([ps (use-compiled-file-paths)])
      (if (pair? ps)
          (car ps)
          (string->path "compiled"))))
  (define-values (_parent name _)
    (split-path p))
  (equal? name expected))
