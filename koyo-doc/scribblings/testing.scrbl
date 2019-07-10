#lang scribble/doc

@(require (for-label racket/base
                     racket/contract
                     web-server/http)
          "koyo.rkt")

@title[#:tag "testing"]{Testing}

@defmodule[koyo/testing]

This module provides utilities for testing request handlers.

@defproc[(make-test-request [#:method method (or/c false/c bytes? string?) "GET"]
                            [#:content content (or/c false/c bytes? string?) #f]
                            [#:headers headers (listof (or/c header?
                                                             (cons/c (or/c bytes? string?)
                                                                     (or/c bytes? string?)))) null]
                            [#:bindings bindings (listof binding?) null]
                            [#:scheme scheme string? "http"]
                            [#:host host string? "127.0.0.1"]
                            [#:port port (integer-in 0 65535) 80]
                            [#:path path string? "/"]
                            [#:query query (listof (cons/c symbol? (or/c false/c string?))) null]
                            [#:client-ip client-ip string? "127.0.0.1"]) request?]{

  Generates @racket[request] values.
}
