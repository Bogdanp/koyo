#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract/base)
          "koyo.rkt")

@title[#:tag "guard"]{Guard}
@defmodule[koyo/guard]

This module provides utilities for implementing early returns from
request handlers and other procedures.

@defform*[((guard expr)
           (guard expr #:else else-expr))]{

  Within a @racket[with-guard] form, ends execution early if
  @racket[expr] is @racket[#f]. If @racket[expr] is not @racket[#f], it
  is returned normally. Use outside of the body of a @racket[with-guard]
  form is a syntax error.

  On early exit, when the first form is used, the result of the
  @racket[with-guard] form is the application of the form's guard
  procedure. When the second form is used, the result of the
  @racket[with-guard] form is @racket[else-expr].

  @history[#:added "0.28"]
}

@defform[(with-guard guard-proc-expr
           body ...+)]{

  Captures an escape continuation that is used to abort the
  execution of @racket[body]s when a guard condition fails. The
  @racket[guard-proc-expr] is a procedure that is executed when a
  @racket[guard] without an @racket[#:else] keyword fails.

  @examples[
    (require koyo/guard)
    (with-guard (lambda () 'not-found)
      (guard #f)
      (displayln "unreachable"))
    (code:line)
    (with-guard (lambda () 'not-found)
      (guard #t)
      (guard #f #:else 42)
      (displayln "unreachable"))
  ]

  @history[#:added "0.28"]
}

@defform[(define-guard (guard-id arg-id ...)
           maybe-else
           guard-expr)
         #:grammar ([maybe-else
                     (code:line)
                     (code:line #:else else-expr)])]{

  Defines a reusable guard.

  @examples[
    (require koyo/guard)
    (define-guard (guard-positive x)
      #:else 'fail-not-positive
      (> x 0))
    (with-guard (lambda () 'fail)
      (guard-positive 5)
      (println 'after-5)
      (guard-positive 0))
  ]

  @history[#:added "0.49"]
}
