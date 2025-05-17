#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/string
                     web-server/http)
          "koyo.rkt")

@title[#:tag "csrf"]{CSRF}

@defmodule[koyo/csrf]

@(define csrf
   (hyperlink "https://en.wikipedia.org/wiki/Cross-site_request_forgery" "CSRF"))

This module provides wrapper procedures for protecting your
application against @csrf attacks.

@defparam[current-csrf-error-handler handler (-> request? response?)]{
  Holds the request handler that is invoked when the request fails CSRF
  validation. The default implementation returns a @emph{403 Forbidden}
  response along with some HTML describing the issue.

  This parameter is used by both @racket[wrap-corf] and
  @racket[wrap-cors].
}

@section{Header-based Approach}

@(define Sec-Fetch-Site
   (hyperlink "https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Sec-Fetch-Site" "Sec-Fetch-Site"))

@(define Origin
   (hyperlink "https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Origin" "Origin"))

The cross-origin request forgery wrapper protects a handler from CSRF
attacks by inspecting the @Sec-Fetch-Site and @Origin headers. This
method is preferred over the older @secref{csrf-token} since it involves
fewer moving parts and does not require the developer to pass a token
with every form submission.

@defproc[(wrap-corf [handler procedure?]) (-> request? any/c ... response?)]{
  Wraps a handler such that any incoming @tt{DELETE}, @tt{PATCH},
  @tt{POST} or @tt{PUT} request whose @tt{Sec-Fetch-Site} header is
  not either @tt{same-site} or @tt{none} and whose @tt{Origin} header
  differs from its @tt{Host} header is rejected by passing the request
  to the value of @racket[current-csrf-error-handler].
}

@section[#:tag "csrf-token"]{Token-based Approach}

@defparam[current-csrf-token-generator generator (-> non-empty-string?)
          #:value generate-random-string]{
  Contains the procedure that is used to generate new CSRF tokens.
}

@defparam[current-csrf-token token (or/c #f non-empty-string?)]{
  Holds the CSRF token for the current request. If the current request
  handler was wrapped with @racket[wrap-csrf], then this is guaranteed
  to contain a non-empty string.
}

@defparam[current-csrf-token-reader reader (-> request? (or/c #f non-empty-string?))]{
  Contains the procedure that is used to extract the current CSRF token
  from the request. The default implementation tries to extract the CSRF
  token from a header called @tt{x-csrf-token} and, if that fails, then
  it tries to get it from a binding called @tt{csrf-token}.
}

@defproc[((wrap-csrf [sessions session-manager?]) [handler procedure?]) (-> request? any/c ... response?)]{
  Wraps a handler such that any incoming @tt{DELETE}, @tt{PATCH},
  @tt{POST} or @tt{PUT} request that doesn't contain a valid
  CSRF token is rejected by passing the request to the value of
  @racket[current-csrf-error-handler].

  CSRF tokens are automatically generated and stored in each users'
  @racket[sessions]. If a user's session already contains a CSRF token,
  then it is reused until the session expires.

  This wrapper must be applied after @racket[wrap-session].
}
