#lang racket/base

(require racket/file
         racket/path)

;; If a module was compiled with enforcement of module constants (the
;; default), it will fail to reload.  This module provides utilities
;; for transparently recovering from those cases.

(provide
 constant-redefined-exn?
 delete-zos!
 mod-path->zo-path
 zo-dir
 zo-dir?
 zo-path?)

(define (constant-redefined-exn? e)
  (and (exn:fail:contract:variable? e)
       (regexp-match? #rx"cannot re-define a constant" (exn-message e))))

(define (delete-zos! root)
  (for ([p (in-list (find-files zo-path? root))])
    (eprintf "  [deleting ~a]~n" p)
    (delete-file p)))

(define (mod-path->zo-path p)
  (define compiled (zo-dir))
  (define-values (dir filename _)
    (split-path p))
  (build-path dir compiled (path-replace-extension filename #"_rkt.zo")))

(define (zo-dir)
  (define ps (use-compiled-file-paths))
  (if (pair? ps)
      (car ps)
      (string->path "compiled")))

(define (zo-path? p)
  (define-values (parent name _)
    (split-path p))
  (and (equal? (path-get-extension name) #".zo")
       (path? parent)
       (zo-dir? parent)))

(define (zo-dir? p)
  (define expected (zo-dir))
  (define-values (_parent name _)
    (split-path p))
  (equal? name expected))
