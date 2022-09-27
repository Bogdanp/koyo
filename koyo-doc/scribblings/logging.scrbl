#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/logging
                     racket/string)
          "koyo.rkt")

@title[#:tag "logging"]{Logging}

@defmodule[koyo/logging]

This module provides utilities for displaying log messages.

@defproc[(start-logger [#:levels levels (listof (cons/c symbol? log-level/c))]
                       [#:color? color? boolean? #t]
                       [#:parent parent logger? (current-logger)]
                       [#:output-port out port? (current-error-port)]) (-> void?)]{

  Starts a background thread that receives logs based on
  @racket[levels] and writes them to @racket[out] in the following
  format:

  @tt{    [<timestamp>] [<pid>] [<level>] <topic>: <message>}

  The @racket[levels] argument is a list of topic and level pairs.
  The @tt{debug}, @tt{info}, @tt{warning} and @tt{error} @tt{<level>}s
  are colored using ANSI escape sequences unless @racket[#:color?] is
  @racket[#f].

  The return value is a function that will stop the background thread
  when called.  Calling the stopper function after the thread has been
  stopped has no effect.
}
