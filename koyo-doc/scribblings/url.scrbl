#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/string
                     web-server/http)
          "koyo.rkt")

@title[#:tag "url"]{URLs}

@defmodule[koyo/url]

This module provides functions for generating URLs in your
application.


@section{Application URLs}

@deftogether[
  (@defparam[current-application-url-scheme scheme (or/c "http" "https") #:value "http"]
   @defparam[current-application-url-host host non-empty-string? #:value "127.0.0.1"]
   @defparam[current-application-url-port port (integer-in 0 65535) #:value 8000])]{

  @racket[make-application-url] uses these parameters when constructing URLs.
}

@defproc[(make-application-url [path-element string?] ...
                               [#:query query (listof (cons/c symbol? string?)) null]
                               [#:fragment fragment (or/c #f string?) #f]) string?]{

  Generates absolute URLs based on the given arguments.
}


@section{Reverse Routing}

@defparam[current-reverse-uri-fn fn (-> symbol? any/c ... string?)]{
  This parameter holds the function that should be used to generate
  reverse URIs.  Generally, you'll take the reverse URI function that
  @racket[dispatch-rules+roles] generates and stick it in here.
  @racket[reverse-uri] will then proxy to it.

  The default implementation raises an error.
}

@defproc[(reverse-uri [where symbol?]
                      [#:query query (listof (cons/c symbol? (or/c #f string?))) null]
                      [arg any/c] ...) string?]{

  Applies the @racket[current-reverse-uri-fn] to @racket[route] and
  @racket[arg ...].
}
