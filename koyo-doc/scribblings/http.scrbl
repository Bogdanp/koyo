#lang scribble/doc

@(require (for-label koyo
                     net/url
                     racket/base
                     racket/contract
                     web-server/http)
          scribble/example
          "koyo.rkt")

@title[#:tag "http"]{HTTP}

@defmodule[koyo/http]

This module provides utilities for working with HTTP-related data
structures.

@defproc[(request-path [req request?]) string?]{
  Returns the absolute request path for @racket[req], scrubbed of path params.
}

@deftogether[
  (@defproc[(bindings-ref [bindings (listof binding?)]
                          [name symbol?]
                          [default (or/c #f string?) #f]) (or/c #f string?)]
   @defproc[(bindings-ref-bytes [bindings (listof binding?)]
                                [name symbol?]
                                [default (or/c #f bytes?) #f]) (or/c #f bytes?)]
   @defproc[(bindings-ref-number [bindings (listof binding?)]
                                 [name symbol?]
                                 [default (or/c #f number?) #f]) (or/c #f number?)]
   @defproc[(bindings-ref-symbol [bindings (listof binding?)]
                                 [name symbol?]
                                 [default (or/c #f symbol?) #f]) (or/c #f symbol?)])]{

  Finds the first binding in @racket[bindings] whose name is
  @racket[name] and returns its value or @racket[default].
}

@defproc[(url-scrub [u url?]) url?]{
  Removes all the path params from @racket[u], while leaving its path
  intact.  This is used by the default continuation mismatch handler
  to strip the current URL of its continuation id.

  @examples[
  #:label #f
    (require koyo/http
             net/url)

    (url->string
     (url-scrub
      (string->url "https://127.0.0.1/foo/bar;(\"k\" . \"123\")/baz")))
  ]
}

@defproc[(url-path* [u url?]) string?]{
  Joins the scrubbed path params in @racket[u] and returns the
  resulting string.

  @examples[
    (require koyo/http
             net/url)

    (url-path* (string->url "https://example.com"))
    (url-path* (string->url "https://example.com/"))
    (url-path* (string->url "https://example.com/a/b/c"))
  ]

  @history[#:added "0.21"]
}

@defproc[(url-has-path? [u url?] [p string?]) boolean?]{
  Returns @racket[#t] when the scrubbed path of @racket[u] matches
  @racket[p].

  @examples[
    (require koyo/http
             net/url)

    (url-has-path? (string->url "https://example.com") "")
    (url-has-path? (string->url "https://example.com") "/")
    (url-has-path? (string->url "https://example.com/a") "/a")
    (url-has-path? (string->url "https://example.com/a/b/c") "/a/b/c")
    (url-has-path? (string->url "https://example.com/a/b/c") "a/b/c")
  ]

  @history[#:added "0.21"]
}
