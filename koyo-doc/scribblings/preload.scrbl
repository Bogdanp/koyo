#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     web-server/http)
          "koyo.rkt")

@title[#:tag "preload"]{Preloading}

@defmodule[koyo/preload]

@(define preload-anchor
   (link "https://www.w3.org/TR/preload/#example-3" "preload"))

This module provides a middleware and facilities for working with
@preload-anchor headers.

@defparam[current-preload-dependencies deps (listof string?)]{
  This parameter holds the set of absolute dependency URLs for the
  current request.
}

@defproc[(track-preload-dependency! [dependency string?]) void?]{
  Adds @racket[dependency] to the @racket[current-preload-dependencies].
}

@defproc[(make-preload-headers) (listof header?)]{
  Converts the @racket[current-preload-dependencies] into a list of
  @tt{Link} headers.
}

@defproc[(wrap-preload [handler (-> request? can-be-response?)]) (-> request? can-be-response?)]{
  Wraps @racket[handler] so that preload dependencies can be tracked
  during its execution.
}
