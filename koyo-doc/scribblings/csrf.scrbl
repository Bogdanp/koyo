#lang scribble/doc

@(require (for-label racket/string
                     web-server/http)
          "koyo.rkt")

@title[#:tag "csrf"]{CSRF}

@defmodule[koyo/csrf]


@(define csrf
   (hyperlink "https://en.wikipedia.org/wiki/Cross-site_request_forgery" "CSRF"))

This module provides a wrapper function for protecting your
application against @csrf attacks.

@defparam[current-csrf-token-generator generator (-> non-empty-string?)
          #:value generate-random-string]{
  Contains the function that is used to generate new CSRF tokens.
}

@defparam[current-csrf-token token (or/c false/c non-empty-string?)]{
  Holds the CSRF token for the current request.  If the current
  request handler was wrapped with @racket[wrap-csrf], then this is
  guaranteed to contain a non-empty string.
}

@defparam[current-csrf-token-reader reader (-> request? (or/c false/c non-empty-string?))]{
  Contains the function that is used to extract the current CSRF token
  from the request.  The default implementation tries to extract the
  CSRF token from a header called @emph{x-csrf-token} and, if that
  fails, then it tries to get it from a binding called
  @emph{csrf-token}.
}

@defparam[current-csrf-error-handler handler (-> request? response?)]{
  Holds the request handler that is invoked when the request does not
  contain a valid CSRF token.  The default implementation returns a
  @emph{403 Forbidden} response along with some HTML describing the
  issue.
}

@defproc[((wrap-csrf [sessions session-manager?]) [handler (-> request? response?)]) (-> request? response?)]{
  Wraps a handler such that any incoming @emph{DELETE}, @emph{POST} or
  @emph{PUT} request that doesn't contain a valid CSRF token is
  rejected by passing the request to @racket[current-csrf-error-handler].

  CSRF tokens are automatically generated and stored in each users'
  @racket[sessions].  If a user's session already contains a CSRF
  token, then it is reused until the session expires.
}
