#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/string
                     web-server/dispatchers/dispatch)
          "koyo.rkt")

@title[#:tag "static"]{Static}

@defmodule[koyo/static]

This module provides utilities for building static file dispatchers.

@defproc[(make-static-dispatcher [root-path path-string?]
                                 [root non-empty-string? "/static/"]) dispatcher/c]{

  Returns a dispatcher that serves files from @racket[root-path] under
  the @racket[root] URI.
}
