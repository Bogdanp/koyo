#lang racket/base

(provide
 find-file-in-project)

(define vcs-dirs (map string->path '(".git" ".hg" ".svn")))
(define (is-project-root? paths)
  (for/or ([p (in-list paths)])
    (member p vcs-dirs equal?)))

(define (find-file-in-project name-str start [max-depth 10])
  (define name (string->path name-str))
  (let loop ([start start] [depth 0])
    (define paths (directory-list start))
    (cond
      [(member name paths equal?)
       (simplify-path (path->complete-path name start))]
      [(is-project-root? paths) #f]
      [(< depth max-depth)
       (loop (build-path start 'up) (add1 depth))]
      [else #f])))
