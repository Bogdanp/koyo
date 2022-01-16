#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/string
                     web-server/http
                     web-server/servlet)
          "koyo.rkt")

@title[#:tag "profiler"]{Profiler}

@defmodule[koyo/profiler]

This module provides an instrumenting profiler that can be used to
measure how much time particular parts of your application are taking
to execute.  It comes with an embeddable widget that you can display
on every page of your application during development.  This is what it
looks like:

@centered[
 (image
  #:scale 1/3
  (build-path media-path "example-profiler.png"))]

Most of the functionality provided by koyo is instrumented using this
profiler.


@defparam[profiler-enabled? enabled? boolean? #:value #f]{
  This parameter controls whether or not profiling is enabled.  If the
  profiler is disabled then no timings will be collected and the
  widget will not be shown.
}

@defproc[(wrap-profiler [handler procedure?]) (-> request? any/c ... response?)]{
  Wraps a request handler for timing instrumentation.
}

@defform[
  (with-timing maybe-label description
    e ...+)
  #:grammar ([maybe-label (code:line)
                          (code:line label-sym)])
  #:contracts ([description non-empty-string?]
               [label-sym symbol?])
]{

  Evaluates @racket[e]s and returns the result.  If profiling is
  enabled then it times how long it takes to run the @racket[e]s and
  associates them with @racket[label-sym] and @racket[description]
  under the parent timing (if any).

  When a @racket[label-sym] is not provided, the label of the
  enclosing @racket[with-timing] block is reused.
}

@defproc[(profile-write) void?]{
  Writes the HTML widget for the current profile into the current
  output port.
}
