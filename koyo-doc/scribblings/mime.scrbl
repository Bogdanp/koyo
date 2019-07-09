#lang scribble/doc

@(require (for-label racket/base
                     racket/contract)
          "koyo.rkt")

@title[#:tag "mime"]{MIME}

@defmodule[koyo/mime]

This module provides utilities for working with MIME types.

@defproc[(path->mime-type [p path?]) (or/c false/c bytes?)]{
  Given a path @racket[p], this function looks up the mime type for
  that path (based on its extension) and returns it.  The backing mime
  type mapping for this function is the same one that the nginx web
  server uses.
}
