#lang scribble/doc

@(require (for-label component
                     crontab
                     koyo
                     racket/base
                     racket/contract
                     racket/string)
          "koyo.rkt")

@title[#:tag "scheduling"]{Scheduling}
@defmodule[koyo/crontab]

This module provides a syntactic form for generating components that
schedule tasks using a @tt{cron}-like syntax.  This functionality is
based on @other-doc['(lib "crontab/crontab-manual.scrbl")].

@(define (ctech text)
  (tech #:doc '(lib "component/component.scrbl") text))

@defform[
  (crontab*
   [schedule-expr handler-expr] ...+)
  #:contracts ([schedule-expr string?]
               [handler-expr (-> exact-integer? any)])
]{
  A variant of @racket[crontab] that produces a @ctech{component}.  See
  the documentation of @racket[crontab] for details.

  @examples[
    (require component koyo)
    (define-system prod
      [app () (lambda () 'the-app)]
      [cron (app) (lambda (app)
                   (crontab*
                    ["* * * * * *"
                     (lambda (timestamp)
                       (printf "~a: ~a~n" timestamp app))]))])
    (system-start prod-system)
    (sleep 5)
    (system-stop prod-system)
  ]
}
