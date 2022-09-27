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
                       [#:parent parent logger? (current-logger)]
                       [#:output-port out port? (current-error-port)]
		       [#:color? color? boolean?]) (-> void?)]{

  Starts a background thread that receives logs based on
  @racket[levels] and writes them to @racket[out] using the following
  format:

  @tt{    [<timestamp>] [<pid>] [<level>] <topic>: <message>}

  The @racket[levels] argument is a list of topic and level pairs.

  The return value is a function that will stop the background thread
  when called.  Calling the stopper function after the thread has been
  stopped has no effect.

  By default, @racket[#:color] is @racket[#t], in which case
  the log messages with the levels debug, info, warning, and
  error are all prepended with a color, helpfully indicating
  their level of severity. If @racket[color?] is
  @racket[#f], the output will not be colored.

}
