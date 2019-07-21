#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     web-server/http)
          "koyo.rkt")

@title[#:tag "http"]{HTTP}

@defmodule[koyo/http]

This module provides utilities for working with @racket[request]
values.

@deftogether[
  (@defproc[(bindings-ref [bindings (listof binding?)]
                          [name symbol?]
                          [default (or/c false/c string?) #f]) (or/c false/c string?)]
   @defproc[(bindings-ref-bytes [bindings (listof binding?)]
                                [name symbol?]
                                [default (or/c false/c bytes?) #f]) (or/c false/c bytes?)]
   @defproc[(bindings-ref-number [bindings (listof binding?)]
                                 [name symbol?]
                                 [default (or/c false/c number?) #f]) (or/c false/c number?)]
   @defproc[(bindings-ref-symbol [bindings (listof binding?)]
                                 [name symbol?]
                                 [default (or/c false/c symbol?) #f]) (or/c false/c symbol?)])]{

  Finds the first binding in @racket[bindings] whose name is
  @racket[name] and returns its value or @racket[default].
}
