#lang scribble/doc

@(require (for-label koyo
                     net/mime-type
                     racket/base
                     racket/contract)
          "koyo.rkt")

@title[#:tag "mime"]{MIME}

@defmodule[koyo/mime]

This module provides utilities for working with MIME types.

@defproc[(path->mime-type [p path?]) (or/c #f bytes?)]{
  An alias for @racket[path-mime-type].  This function is deprecated
  and will be removed in a future version.

  @history[#:changed "0.13" @elem{The implementation of this function
  was extracted into the @tt{mime-type-lib} package.}]
}
