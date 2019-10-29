#lang scribble/doc

@(require (for-label json
                     koyo
                     racket/base
                     racket/contract
                     web-server/http)
          "koyo.rkt")

@title[#:tag "json"]{JSON}

@defmodule[koyo/json]

This module provides utilities for working with @racket[jsexpr?]
values.

@defproc[(response/json [e jsexpr?]
                        [#:code code response-code/c 200]
                        [#:headers headers (listof header?) null]) response?]{

  Produces a @racket[response] that writes @racket[e] to the output as
  a JSON value.  The @racket[#:code] and @racket[#:headers] parameters
  are passed to @racket[response/output] unmodified.  The MIME type of
  the response is always @racket[#"application/json; charset=utf-8"].
}
