#lang racket/base

;; Prior to racket/racket@14f0f86, dynamic-rerequire did not handle
;; transitive dependencies correctly so this module can be used to
;; force it to reload dependencies properly on those versions.

(require racket/list
         racket/match
         "version.rkt")

(provide
 should-touch-dependents?
 touch-dependents)

(define should-touch-dependents?
  (< (version-number) 8002000004))

(define (touch-dependents root mod)
  (define (touch path)
    (eprintf "  [touching ~a]~n" path)
    (with-handlers ([exn:fail:filesystem? void])
      (file-or-directory-modify-seconds path (current-seconds))))
  (for ([path (find-dependents root mod)])
    (touch path)
    (touch (mod-path->zo-path path))))

;; TODO: Handle complete compiled paths.
(define (mod-path->zo-path p)
  (define-values (dir filename _)
    (split-path p))
  (define compiled
    (let ([l (use-compiled-file-paths)])
      (if (pair? l) (car l) "compiled")))
  (build-path dir compiled (path-replace-extension filename #"_rkt.zo")))

(define (find-dependents root mod)
  (define dependents-tree
    (build-dependents-tree (simplify-path root)))

  (let loop ([dependents null]
             [modules (list (simplify-path mod))])
    (match modules
      [(list)
       (remove-duplicates dependents)]

      [(list mod mods ...)
       (define dependents* (hash-ref dependents-tree mod null))
       (loop (append dependents dependents*)
             (append dependents* mods))])))

(define (build-dependents-tree mod)
  (let loop ([dependents (hash)]
             [mods (list mod)]
             [seen (hash)])
    (match mods
      [(list)
       dependents]

      [(list (? (λ (mod) (hash-has-key? seen mod))) mods ...)
       (loop dependents mods seen)]

      [(list mod mods ...)
       (parameterize ([current-load-relative-directory (simplify-path (build-path mod 'up))])
         (define dependencies (find-dependencies mod))
         (define dependents*
           (for/fold ([dependents dependents])
                     ([dependency (in-list dependencies)])
             (hash-update dependents dependency (λ (mods) (cons mod mods)) null)))

         (loop dependents* (append mods dependencies) (hash-set seen mod #t)))])))

(define (find-dependencies mod)
  (for*/fold ([dependencies null])
             ([phase (module->imports mod)]
              [dependency (cdr phase)]
              #:when (local? dependency))
    (cons (resolved-module-path-name (module-path-index-resolve dependency)) dependencies)))

(define (local? mpi)
  (define-values (name _)
    (module-path-index-split mpi))
  (string? name))
