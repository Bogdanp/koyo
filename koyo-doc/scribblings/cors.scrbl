#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/string
                     web-server/http)
          "koyo.rkt")

@title[#:tag "cors"]{CORS}

@defmodule[koyo/cors]

@(define cors
   (hyperlink "https://en.wikipedia.org/wiki/Cross-origin_resource_sharing" "CORS"))

This module provides a wrapper function and parameters for handling
@cors requests.

@deftogether[
  (@defparam[current-cors-origin origin (or/c #f non-empty-string?)
             #:value #f]
   @defparam[current-cors-methods methods (listof non-empty-string?)
             #:value '("HEAD" "DELETE" "GET" "PATCH" "POST" "PUT" "OPTIONS")]
   @defparam[current-cors-headers headers (listof non-empty-string?)
             #:value '("*")]
   @defparam[current-cors-max-age max-age exact-nonnegative-integer?
             #:value 86400]
   @defparam[current-cors-credentials-allowed? allowed? boolean?
             #:value #t])]{

  These parameters determine the behavior of @racket[wrap-cors].  Each
  one maps to one of the CORS response headers.

  When @racket[current-cors-origin] is @racket[#f], the origin is
  based upon @racket[current-application-url-scheme] and
  @racket[current-application-url-host].
}

@defproc[(wrap-cors [handler procedure?]) (-> request? any/c ... response?)]{
  Augments @racket[handler] to add the various necessary bits of
  information to support CORS in its response.  For @emph{OPTIONS}
  requests, @racket[wrap-cors] takes full control of the request.
}
